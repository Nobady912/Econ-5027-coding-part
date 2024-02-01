# you lose the R code is bad?
# tes, just clear,
# just please take the time mto make sure everything is clear
# I am littible bit about be readable
# if you got the code for the 1 to 2 min to run? 
  #explain it with the comamdn 
  # its really hard to make the code is hard to grade if you the do not explain.
  # please have the PDF 

# for any the math writing, it better from it. 
 

# cleaning the enviroment!
rm(list = ls())

# lunch the package
library(MASS)

# a quick asiedeon the null and na values 
# from r docuntation 
# null represent  the null object the sabasece of value 
#na is a logical constant of langeith. iwith contains a missing()
#value indicator 

na_vec <- c(2,4,NA)
null_vec <- c(2,4,NULL)

#Null is maening that is nothign there


#the number in there can be anything, but it should be large 
set.seed(4843924)


#the sql would be you can calacaution the mean value, it will force the na value to be zero
#% part one simulating data for a multicarriate regression model 

#sample size
  n <- 1000 
  
  
# darw a epsilo independent of other random variab les
  eps <- rnorm(n)
  
#convariance matrix
  sigma <- matrix (c(
      2, 1, -0.5,
      1, 2.5, 0,25,
      0,5, -0,25, 1), nrow = 3, ncol=3, byrow = TRUE)
  
  
#draw data
  x <- mvrnorm(n,mu,sigma)
  
#ad a colmu vector of ones to x fro the "constant"
  
  x <_ - cbind(rep(1,n), x)

#display the first ew rows of X for the student
  
  head (x)
    #show me few amount of the matrix
  
#define ture parameter vector: 

  beta_true <- (0.3, 1.2, -0.7, 0.4)

#generate y variable according to linear model: 
  
  y <- x %*% beta_true + eps
  
  
#part two estimate the. regression model
    
    #ols 

  beta_hat <- solve(t(x)%*%x)%*%(t(x)%*%y)

  #solve (A) computes inverse of matrix a
  #ESTIMATE ASYMPOTOTIC VARIANCE
  #compute residuals 
    #eps_hat <- y - x %*% 
  
  P <- X%*%solve(t(x)%*% x)%*%t(x)
  h <- diag(n) - P
  eps <- m%*%y 
  
  
  P<- x %*% solve(t(x)%*%x)%*%(t(x)
  m <- diag(n) - P
  eps_hat2 <- M%*%Y
  
  #degress of freedom (typicall n - p - 1 where "p" is unber of slop)
  #parameters: here length(beta_hat) is p +1)
  
  dof <- n - length(beta_hat)
  sigm_hat_sq <- (1/dof)*sum(eps_hat*eps_hat)
  v_beta_hat <- solve(t(x)%*%x) * sigma_hat_sq
  
  
  
  # part three: hypothesis testing for slop coefficients
  
  #part four
  
  
#The F test
    r2 <- 1 - sum(eps_hat*eps_hat)/sum((y-mean(y)))^2
  
    #ask stdeutns to comement on r square, considerinfg the model we are
    #estimating corresponds exactlty to the population DGP data generated profess. isis higher or 
    #lower than you expect? 


 #set degres of freedom for F test:
  nu1 <- length(beta_hat)-1 #of slop coefficients
  nu1 <- n - length(beta_hat)
  
  #compute F statistic
  F_stat <_ (r2.nu)/((1-r2)/nu))
  F_pval <- 1 - pf(f stat, nu1, nu2)
  
#run linear model (R incluedss constant by defulat)
  beta_hat 






