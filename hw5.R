N<-1000
set.seed(12947)
k<-100
y<-rnorm(n=N)
X<-matrix(rnorm(n=N*k),N)
XPXi<-solve(t(X)%*%X)
olsb<-XPXi%*%t(X)%*%y
uh<-y-X%*%olsb
sigsq<-as.numeric(t(uh)%*%uh)/(N-k-1)
vb<-sigsq*diag(XPXi)
se<-sqrt(vb)
t<-olsb/se
colSums(abs(t)>2)
#' pt(2,900, lower.tail=F): 
#' 2.3% of observations are found above 2, in a t(100) distribution
#'  (2.3% also found below)
#' so expect to see 4.6% of observations reach significance at t=0
#' observed 4 out of 100

t_signif = which(abs(t)>2)
t[t_signif]
X_signif = X[,t_signif]
summary(lm(y~X_signif))

#Call:
#  lm(formula = y ~ X_signif)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.83596 -0.65971 -0.00971  0.68107  3.03105 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.005902   0.031805   0.186   0.8528  
# X_signif1    0.072943   0.031255   2.334   0.0198 *
#   X_signif2   -0.082445   0.032771  -2.516   0.0120 *
#   X_signif3    0.073119   0.030440   2.402   0.0165 *
#   X_signif4   -0.080409   0.031277  -2.571   0.0103 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.004 on 995 degrees of freedom
# Multiple R-squared:  0.02315,	Adjusted R-squared:  0.01922 
# F-statistic: 5.895 on 4 and 995 DF,  p-value: 0.0001089

#' clearly all of the variables whose t valuables happened to be above 2
#'   reached significance in the regression of y on those variables
#' but the magnitudes of the coefficients and the R^2 values are all small


##### QUESTION 2 ####

install.packages('car')
library(car)
library(mvtnorm)
Data = rmvnorm(1000, sigma=matrix(c(1,.7,.5,.7,1,.3,.5,.3,1),nrow=3))
e = rnorm(1000)
Data = cbind(rep(1,1000), Data)
ones = Data[,1]; x = Data[,2]; w = Data[,3]; z = Data[,4]
a = 5; b = .2; c = 2; d = -3
y = a*ones + b * x + c*w + d*z + e 

mod1 = lm(y~x+w+z)
summary(mod1)
bhat = summary(mod1)$coef[2]
bse = summary(mod1)$coef[6]

t = bhat/bse # 4.742847
## one sided t-test:at 95% confidence level, would need t-value of:
#> qt(.95,996)
#[1] 1.646385

# F = (SSRr - SSRur)(n-k-1)/(SSRur(q))
SSRur1 = sum((mod1$residuals)^2)

mod2 = lm(y~w+z)
SSRr1 = sum((mod2$residuals)^2)

F1 = ((SSRr1-SSRur1)*(996)) / (SSRur1*(1))
# F1 = 22.49459

linearHypothesis(mod1,("x = 0")) # F = 22.495


####  QUESTION 3 #####

## test H0: b=c
r = matrix(c(0,1,-1,0))
beta_vec = matrix(c(a,b,c,d))
t(r)%*%beta_vec
beta_vec
beta_hat = mod1$coef
qhat = t(r)%*%beta_hat # = -1.751364
## test stat: (qhat - q)/se(qhat)
## se(qhat) = (r'((s^2)(X'X)^-1)r)^.5
s_sq = sum((mod1$residuals)^2)/996 # = 0.9745621
se_qhat = ( t(r) %*% (s_sq*solve(t(Data)%*% Data)%*%r) )^(0.5)
## se_qhat = 0.08387115

tstat = (qhat-0)/se_qhat # = -20.8816
pt(tstat,996) # 6.70158e-81

linearHypothesis(mod1,("x = w")) # F = 436.04


#### QUESTION 4 ####

## H0: b=0, c=0

mod1 = lm(y~x+w+z)
mod3 = lm(y~z)

SSRur2 = sum((mod1$residuals)^2)
SSRr2 = sum((mod3$residuals)^2)

F2 = ((SSRr2 - SSRur2)*(996)) / (SSRur2 * (2))
# F2 = 2255.065
linearHypothesis(mod1, c("x =0","w = 0"))
## F = 2255.1

## F = (1/m(s^2))(rBhat -q)'([r((X'X)^(-1))r']^-1)(rBhat-q)
# m = 2; Bhat = 'beta_hat'; s^2 = 's_sq'
R2 = matrix(c(0,1,0,0,0,0,1,0),nrow=2,byrow=T)
q2 = c(0,0)
F2mat = (1/(2*s_sq))*t(R2%*%beta_hat - q2)%*%
  (solve(R2%*%solve(t(Data)%*%Data)%*%t(R2))) %*% (R2%*%beta_hat - q2)
# F2mat = 2255.065


#### QUESTION 5 #### 
getwd()
list.files()     
load("nes2004r.RData")
nes_full = nes2004r
names(nes)    

summary(nes$prok)

nesmod1 = lm(nes$prok ~ nes$foraid01)
summary(nesmod1)

nesmod2 = lm(prok~male+foraid01,data=nes)
summary(nesmod2)
## coef on male = -0.04007
nes$female = abs(1-nes$male)

nesmod3 = lm(prok~female+foraid01,data=nes)
summary(nesmod3)
# coef on female = 0.04007
# yes got the obvious result

summary(lm(prok~female+male+foraid01,data=nes))
## gives all NA's under Male (estimate, se, t, and p)

nes$race = factor(nes$racerec,levels=c(0:4),labels=c('w', 'b', 'h', 'a', 'o'))
nes$race_b = factor(nes$racerec,levels=c(1,0,2,3,4), labels=c('b','w','h','a','o'))

nesmod4 = lm(prok~female+foraid01+race,data=nes)
summary(nesmod4)
nesmod5 = lm(prok~female+foraid01+race_b,data=nes)
summary(nesmod5)
# note - different std errors for the two models


## part e
nesmod6 = lm(prok~partyid*race,data=nes)
summary(nesmod6)
nesmod6r = lm(prok~partyid+race,data=nes)
## F = ((SSRr - SSRur)*(1040))/(SSRur(4))
SSRur6 = sum((nesmod6$residuals)^2)
SSRr6 = sum((nesmod6r$residuals)^2)
F6 = ((SSRr6 - SSRur6)*(1040))/(SSRur6*(4))
# F6 = 2.011501

linearHypothesis(nesmod6,names(nesmod6$coef)[grep(":", names(nesmod6$coef))])
# F = 2.0115

## part f
nesmod7 = lm(prok~partyid*race_b,data=nes)
summary(nesmod7)
nesmod7r = lm(prok~partyid+race_b,data=nes)
## F = ((SSRr - SSRur)*(1040))/(SSRur(4))
SSRur7 = sum((nesmod7$residuals)^2)
SSRr7 = sum((nesmod7r$residuals)^2)
F7 = ((SSRr7 - SSRur7)*(1040))/(SSRur7*(4))
# F7 = 2.011501

linearHypothesis(nesmod7,names(nesmod7$coef)[grep(":", names(nesmod7$coef))])
# F = 2.0115

## same F-stat with different base category



###### QUESTION 6 #######

mod8 = lm(prok~partyid + fp_terror01 + age + male,data=nes)
summary(mod8)
mod9 = lm(prok~partyid*fp_terror01 + age + male,data=nes)
summary(mod9)
## not much changes... all signs on coefs remain the same (except for male, but magnitude is extremely small)
#  magnitues of partyid and fp_terror coefs decrease slightly, and the standard errors increase considerably,
#   when the interaction is included



