#Question 2
install.packages("mvtnorm")
library("mvtnorm", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
set.seed(135467)
smat<-matrix(c(1,.5,.5,1),2)
data<-rmvnorm(100,sigma =smat)
x<-data[,1]
eps<-data[,2]
y<-3+7*x+eps
reg<-lm(y~x)
regtest<-lm(x~reg$resid)
summary.lm(regtest)
summary.lm(lm(x~eps))

#Question 3
rm(list = ls())

set.seed(135467)
smat<-matrix(c(1,.5,.5,1),2)
data<-rmvnorm(100,sigma =smat)
x1 <-data[,1]
x2 <-data[,2]
epsilon <- rnorm(100, mean=1, sd=1)
a <- 3
b <- 2
c <- 2
y <- a + b*x1 + c*x2 + epsilon
lm(y~x1+x2)
residuals1 <- residuals(lm(y~x2))
residuals2 <- residuals(lm(x1~x2))
lm(residuals1~residuals2)

#Question 4
Y <- matrix(y,ncol=1)
X <- matrix(c(x1,x2),ncol=2)
solve(t(X)%*%X) %*% t(X) %*% Y

#Question 5, part a
lm(y~x1+x2-1)

#Question 5, part b
ycen <- y - mean(y)
x1cen <- x1 - mean(x1)
x2cen <- x2 - mean(x2)
lm(ycen~x1cen+x2cen-1)
