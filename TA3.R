rm(rist=ls())
set.seed(4843924)
#seed should be large 

#loding reqired pcakge
library(MASS)


#simukat the muticvariate regression in for lopp 

#begin by simulating data for a multiariate repgression model

#sample size 
n <- 1000 

#vector of means 
mu <-c(2,1,-2)

#covariance matrix
sigma <- matrix(c(2, 1, -0.5,
                  1, 2.5, -0.25,
                  -0.5, -0.25, 1), nrow = 3, ncol = 3, byrow = TRUE)

#degine ture parater vector 
beta_true <- c(0.3, 1.2, -0.7, 0.4)

#number of the replication4
R <- 1000





#PREALLOCATE MATRIX TO STORE RESULTS
storemat <- matrix(NA, nrow = R, ncol = length(beta_true))


#for loop

for( r in 1:R){
  #draw espilon indepedent of other random variabels
  e <- rnorm(n)
  #draw x variabels from a multivariate normal
  x <- mvrnorm(n, mu, sigma)
  #ADD A COLUME VECTOR OF ONES TO X FRO THE CONSTANT
  x <- cbind(rep(1,n),x)
  #add a cilkum vector of ones to x fro the constant 
  y <- x%*%beta_true + e
  #ols estimator
  beta_hat <- solve(t(x)%*%x)%*%(t(x)%*%y)
  #storeresults
  storemat[r,] <- t(beta_hat)
}

#if we consider the formular the (xTx)^-1 * x^T*Y

plot(storemat[,2])

evil <-data.frame(storemat)
lm(evil)

rm(list=ls())







