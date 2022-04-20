library(ggplot2)


set.seed(12345)

  
  n <- 80
  
  #generate "true" response
  x  <- runif(n, -2, 2) #generate x
  epsilon <- rnorm(n)
  y <- 1 - 2 * x + x^2 + 2 * x^3+ epsilon
  xy <- data.frame(x=x, y=y)
  mxd <- 15
  
  #test set
  x.new <- seq(-2, 2, by = 0.01)
  y.new.true <- 1 - 2 * x.new + x.new^2 + 2 * x.new^3
  r <- length(x.new) 
  degree <- rep(1:mxd, each = r)
  predicted <- numeric(length(x.new)*mxd)
  new.dat <- data.frame(x=rep(x.new, times=mxd),degree,
                        predicted)
  xy.new <- data.frame(x=x.new, y=y.new.true)
  
  # using predict function to find predicted value y based on predicted model
  
  for (i in 1:mxd)
  {
    new.dat[new.dat$degree==i, 3] <- predict(lm(y~poly(x,i)),
                                             newdata=data.frame(x=x.new))
  }
  

p1 <- ggplot() +
  geom_point(aes(x,y),xy,colour="blue") +
  geom_line(aes(x, predicted),new.dat[new.dat$degree==1,], colour="red") +
  geom_line(aes(x,y), xy.new , colour= "green")+
  scale_x_continuous(breaks = round( seq(-2, 2, by = 0.5), 1)) +
  theme_bw()+
  labs(title="Degree 1")


print(p1)

p2 <- ggplot() +
  geom_point(aes(x,y),xy,colour="blue") +
  geom_line(aes(x, predicted),new.dat[new.dat$degree==3,], colour="red") +
  geom_line(aes(x,y), xy.new , colour= "green")+
  scale_x_continuous(breaks = round(seq(-2, 2, by = 0.5), 1)) +
  theme_bw()+
  labs(title="Degree 3")


print(p2)

p3 <- ggplot() +
  geom_point(aes(x,y),xy,colour="blue") +
  geom_line(aes(x, predicted),new.dat[new.dat$degree==15,], colour="red") +
  geom_line(aes(x,y), xy.new , colour= "green")+
  scale_x_continuous(breaks = round(seq(-2, 2, by = 0.5), 1)) +
  theme_bw()+
  labs(title="Degree 15")

print(p3)

