library(mvtnorm)
library(MASS)
library(ggplot2)
library(dplyr)

set.seed(12345)


############ 1A. ONE SIMULATION compare to RMSE ##################################
########## Simulation: Polynomial
#- True response y = intercept + alpha1*x + alpha2*x^2 + alpha3*x^3 + alpha4*x^4 epsilon
#- Calculate the AIC and BIC in six models: 1st. -> 6th. polynomial
#- One simulation


AIC.BIC.RMSE <- function(sample_size,  alpha0=-2, alpha1=-1, alpha2=3, alpha3=0.1, max.degree = 6)
  {
  x_max= 3
  x_min = -3
  n <- sample_size #call the sample size
  
  #generate "true" response
  x  <- runif(n, x_min, x_max) #generate x
  epsilon <- rnorm(n)
  y <- alpha0+ alpha1 * x + alpha2 * x^2 + alpha3 * x^3 + epsilon
  mxd <- max.degree 
  
  #test set
  x.new <- seq(x_min, x_max, by = 0.1)
  r <- length(x.new) 
  dat.new <- data.frame(x=rep(x.new, times=mxd),
                         degree=rep(1:mxd, each = r),
                         predicted= numeric(r*mxd))
  
  # using predict function to find predicted value y based on predicted model
  
  for (i in 1:mxd)
  {
    dat.new[dat.new$degree==i, 3] <- predict(lm(y~poly(x,i, raw = TRUE)),
                                             newdata=data.frame(x=x.new))
  }
  
  
  rmse.calc <- function(predicted_y ,y){
    sqrt(mean((predicted_y-y)^2))
  }
  results <- data.frame(Criterion = c(rep("AIC", mxd), rep("BIC", mxd) ,rep("RMSE", mxd)),
                       value = numeric(mxd*3),
                       degree=rep(1:mxd, times=3))
  for (i in 1:mxd){
    results[i, 2] <- AIC(lm(y~poly(x,i, raw = TRUE)))
    #results[i+mxd, 2] <- AICc(lm(y~poly(x,i, raw = TRUE)))
    results[i+mxd, 2] <- BIC(lm(y~poly(x,i, raw = TRUE)))
    submodel = dat.new[dat.new$degree==1, 1]
    epsilon2 <- rnorm(r)
    results[i+mxd*2, 2] <- 
      rmse.calc(dat.new[dat.new$degree==i, 3], 
                y= alpha0 + submodel*alpha1+submodel^2*alpha2+submodel^3*alpha3 +epsilon2) 
  }
  return(results)
}


sample8.d4 <-  AIC.BIC.RMSE(sample_size = 8) 

sample32.d4 <-  AIC.BIC.RMSE(sample_size = 32) 

sample128.d4 <-  AIC.BIC.RMSE(sample_size = 128) 

sample512.d4 <-  AIC.BIC.RMSE(sample_size = 512)

sample1024.d4 <- AIC.BIC.RMSE(sample_size = 1024)




sample8.d4p <-  ggplot(data= sample8.d4, 
                        aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
                        geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
                        theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample32.d4p <-  ggplot(data= sample32.d4, 
                           aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
                          geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
                          theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample128.d4p <-  ggplot(data= sample128.d4, 
                      aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
                      geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
                      theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample512.d4p <-  ggplot(data= sample512.d4, 
                      aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
                      geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
                      theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample1024.d4p <-  ggplot(data= sample1024.d4, 
                      aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
                      geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
                      theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

#Insights: 
# -BIC has chosen the correct model as the sample size increased. 
# -AIC has also, however the diff between the AIC values of the model with polynomial degrees 3 and 4 differ very little compare to BIC

#Assumption:
# - The BIC is consistent, but the AIC is not.
# - AIC appears OVERFITTING


   # as in 5.1, comparing predictions should be interesting


############ 1B. ONE SIMULATION compare to RMSE ##################################
########## Simulation: Polynomial
#- True response y = intercept + alpha1*x + alpha2*x^2 + alpha3*sin(x) + alpha4*ln(x) 
#- Calculate the AIC and BIC in six models: 1st. -> 6th. polynomial
#- One simulation


      # 'alpha4' not found, I set it to 0
      # x is possibly <= 0, yet "log" is used

AIC.BIC.RMSE <- function(sample_size , x_max= 7, x_min = 1, alpha0=-2, alpha1=-1, alpha2=1, alpha3=4, alpha4=3, max.degree = 6)
{
  
  n <- sample_size #call the sample size
  
  #generate "true" response
  x  <- runif(n, x_min, x_max) #generate x
  epsilon <- rnorm(n)
  y <- alpha0+ alpha1 * x + alpha2 * x^2 + alpha3 * sin(x) + alpha4 * log(x) + epsilon
  mxd <- max.degree 
  
  #test set
  x.new <- seq(x_min, x_max, by = 0.1)
  r <- length(x.new) 
  dat.new <- data.frame(x=rep(x.new, times=mxd),
                        degree=rep(1:mxd, each = r),
                        predicted= numeric(r*mxd))
  
  # using predict function to find predicted value y based on predicted model
  
  for (i in 1:mxd)
  {
    dat.new[dat.new$degree==i, 3] <- predict(lm(y~poly(x,i, raw = TRUE)),
                                             newdata=data.frame(x=x.new))
  }
  
  
  rmse.calc <- function(predicted_y ,y){
    sqrt(mean((predicted_y-y)^2))
  }
  results <- data.frame(Criterion = c(rep("AIC", mxd), rep("BIC", mxd) ,rep("RMSE", mxd)),
                        value = numeric(mxd*3),
                        degree=rep(1:mxd, times=3))
  for (i in 1:mxd){
    results[i, 2] <- AIC(lm(y~poly(x,i)))
    results[i+mxd, 2] <- BIC(lm(y~poly(x,i, raw=TRUE)))
    submodel = dat.new[dat.new$degree==1, 1]
    epsilon2 <- rnorm(r)
    results[i+mxd*2, 2] <- 
      rmse.calc(dat.new[dat.new$degree==i, 3], 
                y= alpha0 + submodel*alpha1+submodel^2*alpha2 + alpha3 * sin(submodel) + alpha4 * log(submodel)  +epsilon2) 
  }
  return(results)
}


sample8.d4 <-  AIC.BIC.RMSE(sample_size = 8) 

sample32.d4 <-  AIC.BIC.RMSE(sample_size = 32) 

sample128.d4 <-  AIC.BIC.RMSE(sample_size = 128) 

sample512.d4 <-  AIC.BIC.RMSE(sample_size = 512)

sample1024.d4 <- AIC.BIC.RMSE(sample_size = 1024)




sample8.d4p <-  ggplot(data= sample8.d4, 
                       aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
  geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
  theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample32.d4p <-  ggplot(data= sample32.d4, 
                        aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
  geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
  theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample128.d4p <-  ggplot(data= sample128.d4, 
                         aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
  geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
  theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample512.d4p <-  ggplot(data= sample512.d4, 
                         aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
  geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
  theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

sample1024.d4p <-  ggplot(data= sample1024.d4, 
                          aes(x=degree, y=value, group=as.factor(Criterion), color=Criterion)) +
  geom_line()+ geom_point() + scale_x_continuous(breaks=1:6)+ 
  theme_bw()+ xlab("Degree of Polynomial")+ ylab("Values of AIC, BIC, and RMSE")

#Insights: 
# -BIC has chosen the correct model as the sample size increased. 
# -AIC has also, however the diff between the AIC values of the model with polynomial degrees 3 and 4 differ very little compare to BIC

#Assumption:
# - The BIC is consistent, but the AIC is not.
# - AIC appears OVERFITTING

################### 2. AIC and BIC values when increase number of independent variables ###################

# sample size ss = 200
# true model Y = beta1*X1 + beta2*x2 + epsilon (ignore intercept)
# independent variables: X1, ..., Xp
# increase p in range of [0, 200] and find AIC, BIC (Why we choose p = 200? also sample size is 200)
# at each step repeat 100 times, then find mean of 100 values for each AIC and BIC 
# Goal: Test overfitting property between AIC and BIC

AIC.BIC.value <- function(nsims=500, p = 200)
{
  ss <- p
  AIC.value <- rep(0, p)
  BIC.value <- rep(0, p)
  
  result <- data.frame(criterion = c(rep("AIC",p), rep("BIC",p)),
                       no.variable = rep(1:p, 2), 
                       value = numeric(p*2))
  AICresult <- matrix(nrow=nsims, ncol=p)
  BICresult <- matrix(nrow=nsims, ncol=p)
  
  for(j in 1:nsims){
    
    for(pp in 1:p){
      #generate "true" response
      #ss <- 200
      #p <- 200
      X <- matrix(rnorm(ss*p), nrow = ss, ncol = p)
      epsilon <- rnorm(ss, 0, sd=1)
      beta <- c(2, 1, rep(0,p-2))
      beta <- as.matrix(beta)
      
      Y <- drop(X%*%beta+epsilon)
      
      #generate regression model
      reg.data <- data.frame(y=Y,x=X[,1:pp])
      reg.model <- lm(y~., data=reg.data)
      AICresult[j, pp] <- AIC(reg.model)
      BICresult[j, pp] <- BIC(reg.model)
    }
    
  }
  
  
  result[result$criterion=="AIC", 3] <- colMeans(AICresult)
  result[result$criterion=="BIC", 3] <- colMeans(BICresult)
  
  
  return(result)
  
}

result2 <- AIC.BIC.value()
result2p <-  ggplot(data= result2,         # here was result5
                    aes(x=no.variable, y= value, group=as.factor(criterion), color=criterion)) +
  geom_line()+ geom_point() + 
  theme_bw()+ xlab("No. of independent variables")+ ylab("Mean value of AIC/BIC from 500 simulated datasets")

result2p

#Insights: 
# The BIC is more resistant to overfitting than the AIC.


########3. Will not be menntioned - Probability of selecting the true model with regard of changing number of SIMULATION #########################

set.seed(123)
nsim.AIC.BIC <- function(sample_size=500, x_max= 2, x_min = -2, alpha0=-2, alpha1=-1, alpha2=1, alpha3=2, alpha4 = 3,
                         max.degree = 6){
  nsims <-  c(8, 16, 32, 64, 128, 256, 512, 1024, 2048)
  lnsims = length(nsims)
  
  proportion <- data.frame(criterion = c(rep("AIC", lnsims), rep("BIC", lnsims)),
                           probability = numeric(lnsims*2),
                           nsims = rep(nsims, times = 2))
  for (k in nsims){
    #k <- 8
    #x_max <- 2
    #x_min <- -2
    #alpha <- 3
    #beta <- 1
    #gamma <- 3
    #theta <- 0
    #delta <- 0
    #max.degree <- 6
    #Initialize probability of selecting the true the polynomial, in this case is polynomial of degree 3
    AIC.ST <- 0 
    BIC.ST <- 0 
    
    #Call the sample size
    n <- sample_size
    
    for (j in 1:k) {
      
      #generate "true" response
      x  <- runif(n, x_min, x_max) #generate x
      epsilon <- rnorm(n)
      y <- alpha0+alpha1 * x + alpha2 * x^2 + alpha3 * x^3 + alpha4 * x^4 + epsilon #generate model
      mxd <- max.degree 
      
      #test set
      x.new <- seq(x_min, x_max, by = 0.1)
      r <- length(x.new) 
      dat.new <- data.frame(x=rep(x.new, times=mxd),
                            degree=rep(1:mxd, each = r),
                            predicted= numeric(r*mxd))
      
      # using predict function to find predicted value y based on predicted model
      
      for (i in 1:mxd)
      {
        dat.new[dat.new$degree==i, 3] <- predict(lm(y~poly(x,i)),
                                                 newdata=data.frame(x=x.new))
      }
      
      
      AIC.value <- data.frame(Criterion = c(rep("AIC", mxd)),
                              value = numeric(mxd),
                              degree=rep(1:mxd))
      BIC.value <- data.frame(Criterion = c(rep("BIC", mxd)),
                              value = numeric(mxd),
                              degree=rep(1:mxd))
      for (i in 1:mxd){
        AIC.value[i, 2] <- AIC(lm(y~poly(x,i, raw = TRUE)))
        BIC.value[i, 2] <- BIC(lm(y~poly(x,i, raw = TRUE)))
      }
      
      AIC.min <- AIC.value[which.min(AIC.value$value),]
      BIC.min <- BIC.value[which.min(BIC.value$value),]
      
      # If AIC chooses the true polynomial of degree 4 then we increase 1
      if (AIC.min[,3] == 4){
        AIC.ST = AIC.ST + 1
      }
      if (BIC.min[,3] == 4){
        BIC.ST = BIC.ST + 1
      }
      
    }
    proportion[proportion$nsims==k&proportion$criterion =="AIC",2 ] = AIC.ST*100/k
    proportion[proportion$nsims==k&proportion$criterion=="BIC",2] = BIC.ST*100/k
  }
  return(proportion)
}


result3 <- nsim.AIC.BIC(sample_size = 100)
result3p <-  ggplot(data= result3, 
                    aes(x=nsims, y=probability, group=as.factor(criterion), color=criterion)) +
  geom_line()+ geom_point() + 
  theme_bw()+ xlab("No. of simulation")+ ylab("Probability of selecting the true model")

result3p

################################## 4. Change sample size  ##################################################
set.seed(123)

ss.AIC.BIC <- function(nsims = 2000, x_max= 2, x_min = -2, alpha0=-2, alpha1=-1, alpha2=1, alpha3=2, alpha4 = 3,
                         max.degree = 6){
  sample_size <-  c(8, 16, 32, 64, 128, 256, 512, 1024, 2048)
  ls = length(sample_size)
  
  proportion2 <- data.frame(samplesize = rep(sample_size, times = 2),
                            criterion = c(rep("AIC", ls), rep("BIC", ls)),
                            probability = numeric(ls*2))
  for (ss in sample_size){
    #ss <- 8
    #k <- 8
    #nsims <- 500
    #x_max <- 2
    #x_min <- -2
    #alpha <- 3
    #beta <- 1
    #gamma <- 3
    #theta <- 0
    #delta <- 0
    max.degree <- 6
    
    #Initialize probability of selecting the true the polynomial, in this case is polynomial of degree 3
    AIC.ST <- 0 
    BIC.ST <- 0 
  
    
    for (j in 1:nsims) {
      
      #generate "true" response
      x  <- runif(ss, x_min, x_max) #generate x
      epsilon <- rnorm(ss)
      y <- alpha0+alpha1 * x + alpha2 * x^2 + alpha3 * x^3 + alpha4 * x^4 + epsilon #generate model
      mxd <- max.degree 
      
      #test set
      x.new <- seq(x_min, x_max, by = 0.1)
      r <- length(x.new) 
      dat.new <- data.frame(x=rep(x.new, times=mxd),
                            degree=rep(1:mxd, each = r),
                            predicted= numeric(r*mxd))
      
      # using predict function to find predicted value y based on predicted model
      
      for (i in 1:mxd)
      {
        dat.new[dat.new$degree==i, 3] <- predict(lm(y~poly(x,i, raw=TRUE)),
                                                 newdata=data.frame(x=x.new))
      }
      
      
      AIC.value <- data.frame(Criterion = c(rep("AIC", mxd)),
                              value = numeric(mxd),
                              degree=rep(1:mxd))
      BIC.value <- data.frame(Criterion = c(rep("BIC", mxd)),
                              value = numeric(mxd),
                              degree=rep(1:mxd))
      for (i in 1:mxd){
        AIC.value[i, 2] <- AIC(lm(y~poly(x,i, raw = TRUE)))
        BIC.value[i, 2] <- BIC(lm(y~poly(x,i, raw = TRUE)))
      }
      
      AIC.min <- AIC.value[which.min(AIC.value$value),]
      BIC.min <- BIC.value[which.min(BIC.value$value),]
      
      # If AIC chooses the true polynomial of degree 4 then we increase 1
      if (AIC.min[,3] == 4){
        AIC.ST = AIC.ST + 1
      }
      if (BIC.min[,3] == 4){
        BIC.ST = BIC.ST + 1
      }
      
    }
    proportion2[proportion2$samplesize==ss&proportion2$criterion =="AIC",3 ] = AIC.ST/nsims
    proportion2[proportion2$samplesize==ss&proportion2$criterion=="BIC",3] = BIC.ST/nsims
  }
  return(proportion2)
}


result4 <- ss.AIC.BIC()
result4$samplesize <- factor(result4$samplesize)
result4p <-  ggplot(data= result4, 
                    aes(x=samplesize, y=probability, group=as.factor(criterion), color=criterion)) +
                    geom_line()+ geom_point() +
                    theme_bw()+ xlab("Sample size")+ ylab("Probability of selecting the correct polynomial of degree four")

result4p


################### 5. Simulation for multiple linear regression ###################
#MULTIPLE REGRESSION:
# True model: y = X1 + X2 + X3 + epsilon
# X1, X2, ..., Xp ~ N(0, 1) - independent
# epsilon ~ N(0, 1)
# Find the probability of choosing the true model for AIC and BIC while the sample size in the range of (2^l, l = 3, 4, 5, ..., 11)
# no. of simulation: 500

simulation.mlr <- function(nsims=2000, sample_size=c(8,16,32,64,128, 256, 512, 1024, 2048), p = 6)
  
{
  
  
  ls <- length(sample_size)
  
  proportion3 <- data.frame(samplesize = rep(sample_size, times = 2),
                            criterion = c(rep("AIC", ls), rep("BIC", ls)),
                            probability = numeric(ls*2))
  
  for(ss in sample_size){
    
    AIC.ST <- 0
    BIC.ST <- 0
    #nsims <-  50
    for(j in 1:nsims){
      
      #ss <- 256
      #p <- 5
      
      #generate "true" response
      
      X <- matrix(rnorm(ss*p), nrow = ss, ncol = p)
      epsilon <- rnorm(ss, 0, sd=3)
      beta <- c(1, 2, 3, rep(0,p-3))
      beta <- as.matrix(beta)
      Y <- X%*%beta+epsilon
      
      #generate regression model
      reg.data <- data.frame(y=Y,x=X)
      
      reg.model <- lm(y~., data=reg.data)
      
      reg.bestAIC <- stepAIC(reg.model,trace=0, k=2)
      list.varAIC <- colnames(reg.bestAIC$model)
      reg.bestBIC <- step(reg.model,trace=0,k=log(ss))
      list.varBIC <- colnames(reg.bestBIC$model)
      list.varTrue <- c('y', 'x.1', 'x.2', 'x.3')
      if (identical(list.varAIC, list.varTrue)){
        AIC.ST <- AIC.ST + 1
      }
      if (identical(list.varBIC, list.varTrue)){
        BIC.ST <- BIC.ST + 1
      }
      
    }
    
    proportion3[proportion3$samplesize==ss&proportion3$criterion=="AIC",3]=AIC.ST/nsims
    proportion3[proportion3$samplesize==ss&proportion3$criterion=="BIC",3]=BIC.ST/nsims
    
    
  }
  
  return(proportion3)
  
}

result5 <- simulation.mlr()
result5$samplesize <- factor(result5$samplesize)
result5p <-  ggplot(data= result5, 
                    aes(x=samplesize, y=probability, group=as.factor(criterion), color=criterion)) +
                    geom_line()+ geom_point() +
                    scale_x_discrete( labels=c("8", "16", "32", "64", "128", "256", "512", "1024", "2048")) +
                    theme_bw()+ xlab("Sample size")+ ylab("Estimation probability of AIC and BIC selecting correct model")

result5p
#Insights:
# - BIC is consistent
# - Even when n is small, probability of BIC's obtaining the true model rapidly outperforms compare to those of AIC

