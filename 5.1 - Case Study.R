library(ggplot2)
library(readr)
#install.packages("dplyr")
library(dplyr)
library(StepReg)
#install.packages("StepReg")
library(mvtnorm)
library(MASS)

setwd("C:/Users/Thu Beo/Desktop/diploma/Final_submit")
#############################################################################################


#############################################################################################

bodyfat <- read.table('fat.data.txt', header = FALSE, 
                      col.names = c("case", "brozek", "siri", 
                                    "density", "age", "weight_lbs", "height_in", 
                                    "bmi", "fat_free_weight", "neck_cm", "chest_cm", 
                                    "abdomen_cm", "hip_cm", "thigh_cm", "knee_cm", "ankle_cm", 
                                    "biceps_cm", "forearm_cm", "wrist_cm"))
attach(bodyfat)
head(bodyfat)


# 1. Creating a new dataset
Y <- 1/bodyfat$density #Take the response variable as y = 1/D; D is measured body density

X1 <- bodyfat$age
X2 <- bodyfat$weight_lbs
X3 <- bodyfat$height_in
X4 <- bodyfat$neck_cm
X5 <- bodyfat$chest_cm
X6 <- bodyfat$abdomen_cm
X7 <- bodyfat$hip_cm
X8 <- bodyfat$thigh_cm
X9 <- bodyfat$knee_cm
X10 <- bodyfat$ankle_cm
X11 <- bodyfat$biceps_cm
X12 <- bodyfat$forearm_cm
X13 <- bodyfat$wrist_cm


X <- cbind(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) 
df <- data.frame(Y, X)
n <- nrow(df)
S <- var(X)
library(ggplot2)
library(corrplot)
r <- cor(X)
round(r, 2)
corrplot(r, method="color", type ="upper",
         addCoef.col = "black", tl.col = "black", tl.srt=45, 
         sig.level = 0.01, insig = "blank",
         diag=FALSE)

# 2. Finding AIC, BIC manual + function provided in R for full model
bodyfat.lmfull <- lm(Y~., data = df)
bodyfat.ssefull <- sum(resid(bodyfat.lmfull)^2)
p <- 14
n <- nrow(df)
AIC.lmfull <- n + n*log(2*pi) + n*log(bodyfat.ssefull/n) + 2*(p+1)
AIC.lmfull
AIC(bodyfat.lmfull, k=2) 

BIC.lmfull <- n + n*log(2*pi) + n*log(bodyfat.ssefull/n) + log(n)*(p+1)
BIC.lmfull
AIC(bodyfat.lmfull, k=log(n))

# Note: It turns out the same results either using formula or function AIC(...)


# 3. Using backward methods to find the best model by AIC and BIC
AIC.backward <- MASS::stepAIC(bodyfat.lmfull, direction = "backward", k=2)
BIC.backward <- MASS::stepAIC(bodyfat.lmfull, direction = "backward", k=log(n))

summary(AIC.backward)
summary(BIC.backward)

plot(fitted(bodyfat.lmfull) ~ fitted(AIC.backward))
abline(0,1)


plot(fitted(bodyfat.lmfull) ~ fitted(BIC.backward))
abline(0,1)


plot(Y ~ fitted(AIC.backward))
abline(0,1)

cor(fitted(bodyfat.lmfull), fitted(BIC.backward))
cor(fitted(bodyfat.lmfull), fitted(AIC.backward))
cor(Y, fitted(AIC.backward))
cor(Y, fitted(BIC.backward))
cor(Y, fitted(bodyfat.lmfull))

calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

calc_rmse(Y, fitted(AIC.backward))
calc_rmse(Y, fitted(BIC.backward))
### SR: add comparison y with fitted (full, AIC, BIC)

#install.packages("stargazer")
library(stargazer)
#stargazer(AIC.backward$model, BIC.backward$model, title="Comparison of 2 Regression Models", single.row=TRUE)

# 4. Using RMSE to evaluate AIC, BIC, AICc, AR2

set.seed(123456)

calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

RMSE.AIC <- rep(0, 500)
RMSE.AICc <- rep(0, 500)
RMSE.BIC <- rep(0, 500)
RMSE.AR2 <- rep(0, 500)

for (i in 1:500){
  bdf_trn_idx = sample(n, size = trunc(0.7 * n))
  Xtrain <- X[bdf_trn_idx, ]
  Xtest <- X[-bdf_trn_idx, ]
  Ytrain <- Y[bdf_trn_idx]
  Ytest <- Y[-bdf_trn_idx]
  DataTrain <- data.frame(Ytrain, Xtrain)
  DataTest <- data.frame(Ytest, Xtest)
  
  ModelAIC <- step(lm(Ytrain~., data = DataTrain), trace = 0) #set trace = 0 then only final predictor is printed
  ModelBIC<- step(lm(Ytrain~., data = DataTrain), k = log(n*0.7), trace = 0)   #set trace = 0 then only final predictor is printed
  ModelAICc<- step(lm(Ytrain~., data = DataTrain), select = "AICc", trace = 0)
  ModelAR2<- step(lm(Ytrain~., data = DataTrain), select = "adjRsq", trace = 0)
  
  Ypred_AIC <- predict(ModelAIC, newdata = DataTest)
  Ypred_AICc <- predict(ModelAICc, newdata = DataTest)
  Ypred_BIC <- predict(ModelBIC, newdata = DataTest)
  Ypred_AR2 <- predict(ModelAR2, newdata = DataTest)
  
  RMSE.AIC[i] <- calc_rmse(Ytest, Ypred_AIC) 
  RMSE.AICc[i] <- calc_rmse(Ytest, Ypred_AICc)
  RMSE.BIC[i] <- calc_rmse(Ytest, Ypred_BIC)
  RMSE.AR2[i] <- calc_rmse(Ytest, Ypred_BIC)
}


RMSE <- data.frame(criteria = c(rep("AIC", 500), rep("BIC", 500), rep("AICc", 500), rep("AR2", 500)),
                           RMSE.values = c(RMSE.AIC, RMSE.BIC, RMSE.AICc, RMSE.AR2))


ggplot(RMSE[1:1000,], aes(x=criteria,y=RMSE.values, color = criteria)) +
  geom_boxplot() +
  geom_jitter(width=0.25)

summary <- data.frame(Criteria = c("AIC", "BIC", "AICc", "AR2"),
                      Min. = c(min(RMSE.AIC), min(RMSE.BIC), min(RMSE.AICc), min(RMSE.AR2)),
                      Max. = c(max(RMSE.AIC), max(RMSE.BIC), max(RMSE.AICc), max(RMSE.AR2)),
                      Median = c(median(RMSE.AIC), median(RMSE.BIC), median(RMSE.AICc), median(RMSE.AR2)),
                      Mean = c(mean(RMSE.AIC), mean(RMSE.BIC), mean(RMSE.AICc), mean(RMSE.AR2)))


stargazer(summary, title="Comparison of 4 Regression Criteria", single.row=TRUE)


# Result turns that AIC and AICc indicate two similar results and seem to be more accuracy than BIC and AR2.
# Explanation: 
# 1. true model is not included in a real data => BIC has its drawbacks
# 2. sample size is still small => AICc takes advantages and BIC is in constrast


# 5. Simulate new data based on the variance matrix S for fat data X and compare AIC, BIC when increasing sample size

S <- var(X)
beta <- as.matrix(bodyfat.lmfull$coefficients)

list_of_ss <- c(32, 64, 128, 256, 512, 1024)
RMSE.average <- data.frame(Sample.size = list_of_ss,
                           AIC.value =numeric(length(list_of_ss)),
                           BIC.value = numeric(length(list_of_ss)))


for (j in (1:length(list_of_ss))){
  ss <- list_of_ss[j]
  epsilon <- rnorm(ss, 0, 0.01)
  X.new <- cbind(1, mvrnorm(ss, rep(0, 13), S))
  Y.new <- X.new%*%beta + epsilon
  
  nsims <- 500
  RMSE.AIC <- c(rep(0,nsims))
  RMSE.BIC <- c(rep(0,nsims))
  
  for (i in 1:nsim) {
    bdf_trn_idx <- sample(ss, size = trunc(0.7 * ss))
    X.train <- X.new[bdf_trn_idx, ]
    Y.train <- Y.new[bdf_trn_idx, ]
    Data.train <- data.frame(X.train, Y.train)
    X.test <- X.new[-bdf_trn_idx, ]
    Y.test <- Y.new[-bdf_trn_idx, ]
    Data.test <- data.frame(X.test, Y.test)
    
    Model.full <- lm(Y.train~., data = data.frame(X.train, Y.train))
    Model.AIC <- step(Model.full, trace = 0)
    Model.BIC <- step(Model.full, trace = 0, k = log(ss*0.7))
    Ypred.AIC <- predict(Model.AIC, newdata = Data.test)
    Ypred.BIC <- predict(Model.BIC, newdata = Data.test)
    RMSE.AIC[i] <- calc_rmse(Ypred.AIC, Y.test)
    RMSE.BIC[i] <- calc_rmse(Ypred.BIC, Y.test)   
  }

  RMSE.average[j, 2] <- mean(RMSE.AIC)
  RMSE.average[j, 3] <- mean(RMSE.BIC)
  
}


RMSE.average$delta <- RMSE.average$AIC.value - RMSE.average$BIC.value
RMSE.average

# stargazer(RMSE.average, title="Comparison of AIC and BIC when increasing sample size", single.row=TRUE)

# 6. Comparing AIC and BIC when adding more variables `                                                                                                                                                                                                                                                                         `
LNbeta = c(7, 14, 13, 5, 9, 3, 2, 8, 12, 11, 6, 4, 10) #list of nonezero beta 

RMSE.AIC.BIC <- data.frame(corr_beta = LNbeta,
                           RMSE.AIC =numeric(13),
                           RMSE.BIC = numeric(13))

beta <- as.matrix(runif(14, 0, 20))
for(i in 1:13) {
  beta.new <- rep(0,14)
  beta.new[1] <- beta[1] #intercept is always included
  for (j in 1:i)
  {
    beta.new[j+1] <- beta[LNbeta[j]]
  }
  n <- 1000
  X.new <- cbind(1, mvrnorm(n, rep(0, 13), S))
  epsilon <- rnorm(n, 0, 0.1)
  Y.new <- X.new %*% beta.new + epsilon
  
  bdf_trn_idx <- sample(n, size = trunc(0.7 * n))
  X.train <- X.new[bdf_trn_idx, ]
  X.test <- X.new[-bdf_trn_idx, ]
  Y.train <- Y.new[bdf_trn_idx, ]
  Y.test <- Y.new[-bdf_trn_idx, ]
  Data.train <- data.frame(Y.train, X.train)
  Data.test <- data.frame(Y.test, X.test)
  Model.AIC <- step(lm(Y.train~., data = Data.train), trace= 0) #set trace = 0 then only final predictor is printed
  Model.BIC<- step(lm(Y.train~., data = Data.train), k = log(0.7*n), trace = 0)   
  YPred.AIC <- predict(Model.AIC, newdata = Data.test)
  YPred.BIC <- predict(Model.BIC, newdata = Data.test)        
  RMSE.AIC.BIC[i,2] <- calc_rmse(YPred.AIC, Y.test)
  RMSE.AIC.BIC[i,3] <- calc_rmse(YPred.BIC, Y.test)
}

RMSE.AIC.BIC$delta <- RMSE.AIC.BIC$RMSE.AIC - RMSE.AIC.BIC$RMSE.BIC
RMSE.AIC.BIC


# we see that for the strong effects, there is a slight advantage for BIC
# but for the tapering effects, there is a slight advantage for AIC.


######################3. Data food.txt #####################

#realestate = read.table('real estate valuation.txt', header = TRUE)
#attach(realestate)

#X1 <- realestate$age
#X2 <- realestate$lat
#X3 <- realestate$long
#Y <- realestate$price


#n <- length(X1)
#X <- cbind(X1, X2, X3) 
#df <- data.frame(Y, X)

#set.seed(123456)

#calc_rmse = function(actual, predicted) {
#  sqrt(mean((actual - predicted) ^ 2))
#}

# RMSE.AIC <- rep(0, 100)
# RMSE.AICc <- rep(0, 100)
# RMSE.BIC <- rep(0, 100)
# RMSE.AR2 <- rep(0, 100)
# 
# for (i in 1:100){
#   bdf_trn_idx = sample(n, size = trunc(0.7 * n))
#   Xtrain <- X[bdf_trn_idx, ]
#   Xtest <- X[-bdf_trn_idx, ]
#   Ytrain <- Y[bdf_trn_idx]
#   Ytest <- Y[-bdf_trn_idx]
#   DataTrain <- data.frame(Ytrain, Xtrain)
#   DataTest <- data.frame(Ytest, Xtest)
#   
#   ModelAIC <- step(lm(Ytrain~., data = DataTrain), trace = 0) #set trace = 0 then only final predictor is printed
#   ModelBIC<- step(lm(Ytrain~., data = DataTrain), k = log(n), trace = 0)   #set trace = 0 then only final predictor is printed
#   ModelAICc<- step(lm(Ytrain~., data = DataTrain), select = "AICc", trace = 0)
#   ModelAR2<- step(lm(Ytrain~., data = DataTrain), select = "adjRsq", trace = 0)
#   
#   Ypred_AIC <- predict(ModelAIC, newdata = DataTest)
#   Ypred_AICc <- predict(ModelAICc, newdata = DataTest)
#   Ypred_BIC <- predict(ModelBIC, newdata = DataTest)
#   Ypred_AR2 <- predict(ModelAR2, newdata = DataTest)
#   
#   RMSE.AIC[i] <- calc_rmse(Ytest, Ypred_AIC) 
#   RMSE.AICc[i] <- calc_rmse(Ytest, Ypred_AICc)
#   RMSE.BIC[i] <- calc_rmse(Ytest, Ypred_BIC)
#   RMSE.AR2[i] <- calc_rmse(Ytest, Ypred_BIC)
# }
# 
# 
# RMSE <- data.frame(criteria = c(rep("AIC", 100), rep("BIC", 100), rep("AICc", 100), rep("AR2", 100)),
#                    values = c(RMSE.AIC, RMSE.BIC, RMSE.AICc, RMSE.AR2))
# 
# 
# ggplot(RMSE, aes(x=criteria,y=values, color = criteria)) +
#   geom_boxplot() +
#   geom_jitter(width=0.25)
# 
# # When sample size increases, BIC now performs better
# 
# 
