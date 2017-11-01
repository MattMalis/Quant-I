#install.packages('lmtest')
library(lmtest)
library(sandwich)
library(car)
set.seed(1000)

### creating X, errors, y
n=100
x = rnorm(n)
e_homo = rnorm(n)
e_het1 = c(rnorm(50,0,1),rnorm(50,0,10))
e_het2 = rnorm(n,0,x^2)

b0 = 3; b1 = 10
y1 = b0 + b1*x + e_homo
y2 = b0  +b1*x + e_het1
y3 = b0 + b1*x + e_het2
ones = rep(1,100)
X = cbind(ones,x)

### models: 
mod1 = lm(y1~x)
mod2 = lm(y2~x)
mod3 = lm(y3~x)


## mod1 - homoskedastic:
coeftest(mod1)
coeftest(mod1, vcov=hccm)
coeftest(mod1, vcov. = vcovHC(mod1, 'HC3'))
## HC3 gives same results as hccm (HC3 is default arg)

## mod2 - heteroskedastic, variance independent of x
# HCSEs:
coeftest(mod2)
coeftest(mod2, vcov=hccm)

## WLS:
wmod2 = lm(y2~x, weights = c(rep(1, 50), rep(10^(-.5),50)))
summary(wmod2)
summary(mod2)



## mod3 - heteroskedastic, variance a function of x
## HCSEs:
coeftest(mod3)
coeftest(mod3, vcov=hccm)

## WLS:
wmod3 = lm(y3~x, weights = 1/x^2)
summary(wmod3)
summary(mod3)


### sandwich by hand:
bread = solve(t(X)%*%X)
res1 = mod1$residuals
res2 = mod2$residuals
res3 = mod3$residuals

## making the meats
meat1 = matrix(c(rep(0,4)),nrow=2)
for(i in 1:n){
  temp = ((res1[i])^2)*(X[i,]%*%t(X[i,]))
  meat1 = meat1 + temp
}
meat2 = matrix(c(rep(0,4)),nrow=2)
for(i in 1:n){
  temp = ((res2[i])^2)*(X[i,]%*%t(X[i,]))
  meat2 = meat2 + temp
}
meat3 = matrix(c(rep(0,4)),nrow=2)
for(i in 1:n){
  temp = ((res3[i])^2)*(X[i,]%*%t(X[i,]))
  meat3 = meat3 + temp
}


### mod1 - checking the sandwiches
vcov1 = bread%*%meat1%*%bread
se1 = sqrt(vcov1)
se1
coeftest(mod1)
coeftest(mod1,vcov=sandwich)
coeftest(mod1,vcov=vcovHC(mod1, 'HC3'))

### mod2 - checking the sandwiches
vcov2 = bread%*%meat2%*%bread
se2 = sqrt(vcov2)
se2
coeftest(mod2)
coeftest(mod2,vcov=sandwich)
coeftest(mod2,vcov=vcovHC(mod2, 'HC3'))

### mod3 - checking the sandwiches
vcov3 = bread%*%meat3%*%bread
se3 = sqrt(vcov3)
se3
coeftest(mod3)
coeftest(mod3,vcov=sandwich)
coeftest(mod3,vcov=vcovHC(mod3, 'HC3'))

