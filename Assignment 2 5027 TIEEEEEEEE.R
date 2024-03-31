####################################
# 5020 assisgnment 2 - question 3
#Tie Ma 
ß


#Q3 - 2
########
#settitng the enviroment!
rm(list = ls())

#set the direction
setwd("~/Documents/GitHub/Econ-5027-coding-part")

#########

#Q3-B
#set the variance matrix
ultra_evil_variance_matrix <- matrix(c(1,   0.5, 0, 
                                       0.5, 1,   0,
                                       0,   0,   1), nrow = 3, ncol = 3, byrow = TRUE)

#set up the mu
mu <- c(0, 0, 0)

# c sequence (from -1 to 1 by 0.1)
c_value <- seq(from = -1, to = 1, by = 0.05)

# Number of simulation rounds
R <- 1000

# Parameters 
# the n is for the data generate process within the loop
# the n_value of for the loop to keep going.
n_value <- c(100, 250, 500, 1000)

#the t-test with 0.05 degree of freedom (do not change to a you will lost it)
alpha <- 0.05

#the evil pi
pi1 <- -0.3
pi2_value <- c(-0.4, -0.2, -0.1, -0.05, -0.025, 0)



#final_matrix
Final_matrix <- matrix(0, ncol = length(c_value), nrow = length(pi2_value))
rownames(Final_matrix) <- pi2_value



for (pi2_idx in seq_along(pi2_value)) {
  pi2 <- pi2_value[pi2_idx]
  TF_matrix <- matrix(NA, ncol = length(c_value), nrow = R)
  
  for (j in 1:R) {
    n <- 1000
    res <- mvrnorm(n, mu = mu, Sigma = ultra_evil_variance_matrix ) # its should be "Sigma" not "sigma"
    
    #extract residual vectors 
    u <- res[,1] #second stage
    v <- res[,2] #first stage
    
    #z time!
    z_redbull <- rnorm(n, mean = 0, sd = 1)
    #add a colume vector of one to z for the "constant"
    z <- cbind(rep(1,n), z_redbull)
    
    #generate the endogenous X variable
    pi<- c(pi1, pi2)
    x_babyyoda <- z %*% pi + v #so the babyyoda will not mixed with next step!
    
    #add a column vector of ones to x for the "constant" 
    # Add intercept (we now turn the x from 3x3 matrix to 3x4 matrix)
    x <- cbind(rep(1,n), x_babyyoda)
    
    #set up the beta_ture
    beta_true <- c(-0.7, 0)  
    y <- x %*% beta_true + u
    
    
    # IV estimator
    beta_iv <- solve(t(z)%*%x)%*%t(z)%*%y
  
    
    y_hat <- x %*% beta_iv
    eps_hat <- y - y_hat
    
    #the degree of freedom 
    dof <- n - length(beta_iv)
    #残差 the mu
    sigma_hat_sq <- (1 / dof) * sum(eps_hat^2)
    #estimate of the 
    #v_beta_hat <- solve(t(x) %*% x) * sigma_hat_sq
    v_beta_hat <- solve(t(x)%*%z%*%solve(t(z)%*%z)%*%t(z)%*%x)*as.numeric(sigma_hat_sq)
    # standard error time!
    diag_v_beta_hat <- diag(v_beta_hat)
    sd <- sqrt(diag_v_beta_hat)
    
    #the T- test time!
    for (k in 1:length(c_value)) {
      #T statistic for the beta3
      t_st <- (beta_iv[2] - c_value[k])/sd[2]
      #T test P value 
      t_p_value <- 1 - pt(abs(t_st), dof) #using the TA's note!
      #the matrix time!
      #count the p_value
      TF_matrix[j, k] <- t_p_value < 0.05
      #this things was crashed my mac for times。
      #I spend 2 hours on this line a lone
    }
    
    
    Final_matrix[pi2_idx, ] <- colMeans(TF_matrix)
    #and other hour on this line.
  }
  #god tis finally end, I rest in peace.
}

#change the row name of the matrix 
rownames(Final_matrix) <- pi2_value




# Color palette that is friendly for color-blind individuals
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

# Plotting the results
plot(c_value, Final_matrix[1, ], type = "l", lwd = 2, 
     ylim = range(Final_matrix), xlab = "Value of c", 
     ylab = "Proportion of Rejections", 
     main = "Effect of Weak Instruments on Hypothesis Testing",
     col = colors[1])

# Add lines for each pi2 value
for (i in 2:nrow(Final_matrix)) {
  lines(c_value, Final_matrix[i, ], col = colors[i], lwd = 2)
}

# Add a legend
legend("bottomleft", legend = paste("pi2 =", rownames(Final_matrix)), 
       col = colors, lty = 1, cex = 0.8, lwd = 2)




