rm(list=ls())
library(MASS)

setwd("/home/martin/Dropbox/NYU Fall 2017/Quant I/pset2")

# PART 1
correlation = c(-1,0,1)
N = 50
for (corr in correlation) 
  {
  bivn <- mvrnorm(5000, mu = c(0, 0), Sigma = matrix(c(1, corr, corr, 1), 2))
  densityhat <- kde2d(bivn[,1], bivn[,2], n = N)
  png(paste('contour_',corr,',.png'))
  contour(densityhat)
  dev.off()
  png(paste('density_',corr,',.png'))
  persp(densityhat, phi = 30, theta = 0, main = paste("Density estimation with cov(x,y) = ",corr),xlab = "x",ylab = "y",zlab = "f(x,y)")
  dev.off()
}

# PART 2
K = c(10,30,100,1000)
Kgrid = seq(1, length(K), 1)
ntrials = 5
p <- 0.9
number_of_means = c(100,1000)
for (N in number_of_means) 
{
  Ngrid = seq(1, N, 1)
  for (ki in Kgrid) 
  {
    k = K[ki]
    bernoulli_mean_dist = numeric(N)
    binom_mean_dist = numeric(N)
    cauchy_mean_dist = numeric(N)
    for (n in Ngrid) 
    {
      bernoulli_mean_dist[n] <- mean(rbinom(k,1,p))
      binom_mean_dist[n] <- mean(rbinom(k,ntrials,p))
      cauchy_mean_dist[n] <- mean(rcauchy(k)) 
    }
    png(paste('Bernoulli n=',N,', k=',k,'.png'))
    plot(density(bernoulli_mean_dist), main = paste("Bernoulli density with k=",k))
    dev.off()
    png(paste('Binomial n=',N,', k=',k,'.png'))
    plot(density(binom_mean_dist), main = paste("Binomial density with k=",k))
    dev.off()
    png(paste('Cauchy n=',N,', k=',k,'.png'))
    plot(density(cauchy_mean_dist), main = paste("Cauchy density with k=",k))
    dev.off()
  }
}

# PART 3

A = c(seq(0, 100, 10),10^7)
Eps = c(0.01,0.000001,10^(-100))

for (a in A)
{
  print(solve(matrix(c(1,0,a,1),2)))
}

# PART 4
M = matrix(c(1,2,3,1,0,0,0,1,0),nrow = 3)
M_inv = solve(M)

print(M%*%M_inv)
print(M_inv%*%M)