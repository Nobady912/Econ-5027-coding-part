#settitng the enviroment!
rm(list = ls())

#set the direction
setwd("~/Documents/GitHub/Econ-5027-coding-part")

#Lode the all package that necessary.
#some of the packages may not been used in this homework.
#I just copy it around
library("MASS") 
library("ggplot2")

#set the seed
set.seed(101310927)


#load the package
library(MASS)

### part one: simulate regression with endogeneity
#The goal here is to simulate the model
   #y = x*beta + u
   #x = z*pi + v

#where z is an instrument for x, and the error (u,v) are correlated
#producing endogeneity of x in the first equation. 


#sample size
n <- 1000

#generate instrument
z_redbull <- rnorm(n)
#

#add a column vector of one to Z for the "constant"
z <- cbind(rep(1,n), z_redbull)

#generate the residuals for the first and the second stage regerssions
#vector of means:
mu <- c(0, 0)

#covariance matrix 
sigma <- matrix(c(1, 0.5,
                  -0.5, 1), nrow = 2, ncol = 2)


#simulate residuals for the first and second stage regression
res <- mvrnorm(n, mu, sigma)

#extract residual vectors
u <- res[,1] #second stage
v <- res[,2] #first stage

#generate the endogenous X variable
pi <-c(-0.3, 1.2)
x_babyyoda <- z %*% pi + v

#add a column vector of ones to X for the "constant"
x <- cbind(rep(1,n), x_babyyoda)

#the ture beta value
beta <- c(-0.3, 0.4)
y <- x%*%beta + u

#note the slop estimate
beta_ols <- solve(t(x)%*%x) %*% t(x) %*% y

#note the slop estimate
#note that this model is just "identified" therefore, we can use the IV estimator
#IV can be implemented when the model is over identified, but not when it's under identified
beta_iv <- solve(t(z)%*%x) %*% t(z) %*% y


#the IV estimator is this case is also equal to the 2SLS estimator 
#when the model is jsut-identified
pz <- z%*%solve(t(z)%*%z)%*%t(z)
beta_2sls <- solve(t(x)%*%pz%*%x) %*%t(x) %*% pz %*% y








