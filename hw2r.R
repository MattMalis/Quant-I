rm(list=ls())
## this should be on github now
#### QUESTION 2 ####

## three types of random variables: 
## Bernoulli, Binomial with five flips, and Cauchy
## repeat with n = 10, 30, 100, 1000

## plot settings: 
par(mfrow=c(4,3), oma=c(1,1,1,1),mar = c(rep(2,4)))
runs = 500
for (reps in c(10, 30, 100, 1000)){
  print(paste('number of reps:', reps))
  ## bernoulli: 
  bern_sums = numeric(runs)
  for (run in 1:runs){
    vals = rbinom(reps, 1, .5)
    bern_sums[run] = sum(vals)
  }
  plot(density(bern_sums), main = paste0('Bernoulli, n=',reps),xlab='',ylab='')
  
  ## binomial:
  binom_sums = numeric(runs)
  for(run in 1:runs){
    vals = rbinom(reps, 5, .5)
    binom_sums[run] = sum(vals)
  }
  plot(density(binom_sums), main = paste0('Binomial, n=',reps),xlab='',ylab='')
  
  ## cauchy:
  cauchy_sums = numeric(runs)
  for(run in 1:runs){
    vals = rcauchy(reps)
    cauchy_sums[run] = sum(vals)
  }
  plot(density(cauchy_sums), main = paste0('Cauchy, n=',reps),xlab='',ylab='')

}


#### QUESTION 3 ####

for(a in c(seq(0,100,10),100000)){
  print(paste('a:',a))
  mat = matrix(c(1,0,a,1),nrow=2,byrow=F)
  mat_inv = solve(mat)
  print(mat_inv)
  cat('\n')
}


#### QUESTION 4 ####

#a
X =  matrix(c(1,2,3,1),nrow=2,byrow=F)
Z = solve(X)
Z

#b
X =  matrix(c(1,2,-1,-2),nrow=2,byrow=F)
Z = solve(X)
#Error in solve.default(X) : 
#Lapack routine dgesv: system is exactly singular: U[2,2] = 0

#c
M = matrix(c(1,2,2,1,1,0),nrow=3,byrow=F)
N = matrix(c(0,2,1,3,1,4), nrow=2, byrow=F)
M%*%N

#f
A = matrix(c(1,2,3,1,0,0,0,1,0),nrow=3, byrow = F)
A
solve(A)%*%A
A%*%solve(A)
