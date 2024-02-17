#this is the fourth TA seasaton 

library("MASS")
library("sandwich")

setwd("~/Documents/GitHub/Econ-5027-coding-part")
#sample size
n <- 10000

#vector of means:
mu< c(2,1,-2)

#covairance matrix
sigma <- matrix(c(2, 1, -0.5
                  1, 2.5, -0.25,
                  -0.5, -0.25, 1,
                  nrow = 3,
                  ncol = 3,
                  byrow = TRUE))


#draw data
x <- mvrnorm(n, mu,sigma)


#add a colum vector of once to x for the constant 
x <- dbind(rep(1,n), x)

#produce a heteroskedastic error term 
heterosledastic_error <- rnorm(n, mean = 0, sd = sqrt(sqrt1:n) *1.7)

#generate Y
Y <- x%*% beta_ture + heteroskedastic_error

#plotting some of our data
plot(Y)
plot(heteroskedastic_error)
plot(x[,3], Y)

#part two produce heteroskedastic robus estimate of var

#compute beta_ha
beta_hat <- solve (t(x)%*%x)%*%t(x)%*%y

#compute residuals
eps_hat <- y - x%*%beta_hat
eps_hat_squared <- diag(eps_hat%*%t(eps_hat))

#note
dig(eps_hat%*%t(eps)) == eps_hat * eps_hat

#compute heteroskedastic robust erros
robust_cov <- solve(t(x)%*%x)%*%t(x)*eps_hat_squared
rovust_cov <- robust_cov%*%x*%*%solve(t(x)%*%x)
robust_cov <- diag(robust_cov)

#compute heteroskedastidc rpbist errpr. woth sandwich
#the argument hco usew hite 1980 estimator
model <- lm(y ~ x[,2] + x[,3] + x[,4])
robust_var_sandwich <- diag(vcovHC(model, type = "HCO"))


#they using the HC3 so you need to be spcieic to test if the HCO


#compare the results from the package veriss piro mplementation? 
#tan those produce form teh residuals of the lm() function

residuals(model) == eps_hat
roud(residuals(model),9) == round(eps_hat,9)

### part threebootstrap sampling 

#note we have 10,000 unique value in x[,2]
length(unique(x, [,2]))


#talking a bootstrp sample of the second coluime of x
x2_ bootstap <- sampl(x[,2], replace = T)


#ckechking to see how many unque values fo#
#x[,2] appear in the bootstrap saming

counter <-0 
for (x in [,2]){
  if (x %in% x2_bootstrap){
    counter <- counter +1
  }
}

for (size in 1:length(sample_sizes){
  print(sample_size[size])
  print(size)
}









