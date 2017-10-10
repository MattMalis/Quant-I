library(mvtnorm)
rm(list=ls())

###### QUESTION 2 ######

## creating nested list to store values
## outer list corresponds to different covariance values; inner list to different coefficients
covs = c(.2, .5, .8, .9)
vals = vector('list', 4)
names(vals) = c('cov.2', 'cov.5', 'cov.8', 'cov.9')
for(i in  1:4){
  vals[[i]] = vector('list', 6)
  names(vals[[i]]) = c('x_reg_coef', 'z_reg_coef', 'coef_mat', 'x_reg_var', 'z_reg_var', 'var_mat')
}

## looping over covariance values
for(i in 1:4){
  cov = covs[i]
  print(paste("Covariance:",cov))
  ## creating covariance matrix and generating data
  smat<-matrix(c(1,cov,cov,1),2)
  data<-rmvnorm(100,sigma =smat)
  x<-data[,1]
  z<-data[,2]
  ones = rep(1,100)
  y<-2+x+z+rnorm(n=100)
  
  #checking covariance between x and z 
  #print(paste("x~z, coef: ", summary(lm(x~z))$coef[2]))
  
  ## storing residuals and sum of squared residuals
  resids = summary(lm(y~x+z))$residuals
  ssr = sum(resids^2)
  
  #print(summary(lm(y~x+z))$coef)
  ## storing coef and var values from regression output
  x_reg_coef = summary(lm(y~x+z))$coef[2]
  z_reg_coef = summary(lm(y~x+z))$coef[3]
  x_reg_var = (summary(lm(y~x+z))$coef[5])^2
  z_reg_var = (summary(lm(y~x+z))$coef[6])^2
  
  # calculating coef and var matrices manually
  X = cbind(ones, x,z)
  coef_mat = solve(t(X)%*%X)%*%(t(X)%*%(y))
  #z_mat_coef = (solve(t(z)%*%z))*(t(z)%*%(y-(x*x_reg_coef)))
  var_mat = (solve(t(X)%*%X)*(ssr/97))
  #z_mat_var = ((solve(t(z)%*%z))*(ssr/97))^(0.5)
  print((t(X)%*%X))
  # storing all coef and var values in the vals list
  for(j in 1:6){
    vals[[i]][[j]] = list(x_reg_coef, z_reg_coef, coef_mat, x_reg_var, z_reg_var, var_mat)[[j]]}
}



####### QUESTION 3 #######

rm(list=ls())
par(mfrow=c(4,4), oma=c(1,1,1,1),mar = c(rep(2,4)))

error_types = c('normal', 't(df=3)', 'cauchy', 'unif[-1,1]')
reps = 1000

for (n in c(30, 60, 120, 500)) {
  beta_hats = matrix(numeric(4*reps),ncol=4)
  beta_ses = matrix(numeric(4*reps),ncol=4)
  colnames(beta_hats) = c('n', 't', 'u', 'c')
  colnames(beta_ses) = c('n', 't', 'u', 'c')
  
  for(r in 1:reps){
    eps_n = rnorm(n)
    eps_t = rt(n,df=3)
    eps_u = runif(n, -1, 1)
    eps_c = rcauchy(n)
    eps_list = list(eps_n, eps_t, eps_c, eps_u)
    
    x = rnorm(n)
    
    for(i in 1:4){
      eps = eps_list[[i]]
      y = x +eps
      mod = lm(y~x)
      bhat = summary(mod)$coef[2]
      bhat_se = summary(mod)$coef[4]
      
      beta_hats[r,i] = bhat
      beta_ses[r, i] = bhat_se
    }
  }
  for(i in 1:4){
    plot(density(beta_hats[,i]), main = paste('b_hats,', error_types[i],'errors, n =',n))
    
  cat(error_types[i],'errors, n =',n,': \n  mean(beta_hats) =', round(mean(beta_hats[,i]),4),
      '\n  mean(beta_ses) =', round(mean(beta_ses[,i]),4),
      '; sd(beta_hats) =', round(sd(beta_hats[,i]),4),'\n')
  }
  }
