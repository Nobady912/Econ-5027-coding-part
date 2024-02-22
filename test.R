library("MASS")

#clean the environment for the the assignment. 
rm(list = ls())
set.seed(101310927)

#set the variance matrix
evil_variance_matrix <- matrix(c(1.0964, -0.5313, -0.5730, 
                                 -0.5313, 0.9381, -0.4184,
                                 -0.5730, -0.4184, 1.0228), nrow = 3, ncol = 3, byrow = TRUE)

#set up the mu
mu <- c(1.1141, -0.6768, 3.3521)

# c sequence (from -1 to 1 by 0.1)
c_value <- seq(from = -1, to = 1, by = 0.1)

# Number of simulation rounds
R <- 1000

# Parameters 
# the n is for the data generate process within the loop
# the n_value of for the loop to keep going.
n_value <- c(100, 250, 500, 1000)

#the t-test with 0.05 degree of freedom (do not change to a you will lost it)

storage_matrix <- matrix(0, nrow = length(n_value), ncol = 3)

for (k in 1:length(n_value)){ 
  
   pval_matrix <- matrix(0, nrow = R, ncol = 2)
  #rest the n
  n <- n_value[k]
  
  for (i in 1:R){
    
    #data generate process
    simulated_data <- mvrnorm(n, mu, evil_variance_matrix)
    
    #The following code are specific for the model 0.4.
    simulated_data <- mvrnorm(n, mu, evil_variance_matrix)
    #note: here the data does not have the intercept
    # beta1 + beta 2 + beta 3
    
    #take the beta1, beta2 and beta3 out and repute them together. 
    #and I can repose it for the rest of the question 3!
    beta_one <- simulated_data[, 1]
    beta_two <- simulated_data[, 2]
    beta_three <- simulated_data[, 3]
    
    #the model 0.4 have intercept, beta_2 and beta_3
    x <- cbind(rep(1, n), simulated_data[, 2], simulated_data[, 3])

    e <- rnorm(n, mean = 0, sd = 1)  #normal distribution 
    #now its the time for the calculate the y!
    beta_very_true <- c(-0.8, 0, 0.1)  # As a vector 
    
    y <- x %*% beta_very_true + e
    
    # OLS estimation
    beta_hat <- solve(t(x) %*% x) %*% (t(x) %*% y)
    eps_hat <- y - x %*% beta_hat
    
    #the degree of freedom 
    dof <- n - length(beta_hat)
    #残差 the mu
    sigma_hat_sq <- (1 / dof) * sum(eps_hat^2)
    #estimate of the 
    v_beta_hat <- solve(t(x) %*% x) * sigma_hat_sq
    # standard error time!
    diag_v_beta_hat <- diag(v_beta_hat)
    sd <- sqrt(diag_v_beta_hat)
    
    #the T- test time!
      #T statistic for the beta2
      t_st <- (beta_hat[3,])/sd[3]
      #T test P value 
      pval_matrix[i, 1] <- 1 - pt(abs(t_st), dof) #using the TA's note!
      #the matrix time!
      #count the p_value
    
    # Compute and save the p-values
    
##################################

    
#The model 0.5
  
    #take the beta1, beta2 and beta3 out and repute them together. 
    #and I can repose it for the rest of the question 3!
  
    x <- cbind(rep(1, n), simulated_data[, 2])
    
    e <- rnorm(n, mean = 0, sd = 1)  #normal distribution 
    #now its the time for the calculate the y!
    beta_very_true <- c(-0.8, 0)  # As a vector 
    
    y <- x %*% beta_very_true + e
    
    # OLS estimation
    beta_hat <- solve(t(x) %*% x) %*% (t(x) %*% y)
    eps_hat <- y - x %*% beta_hat
    
    #the degree of freedom 
    dof <- n - length(beta_hat)
    #残差 the mu
    sigma_hat_sq <- (1 / dof) * sum(eps_hat^2)
    #estimate of the 
    v_beta_hat <- solve(t(x) %*% x) * sigma_hat_sq
    # standard error time!
    diag_v_beta_hat <- diag(v_beta_hat)
    sd <- sqrt(diag_v_beta_hat)
    
    #the T- test time!
    #T statistic for the beta2
    t_st <- (beta_hat[2,])/sd[2]
    #T test P value 
    pval_matrix[i, 2] <- 1 - pt(abs(t_st), dof) #using the TA's note!
    #the matrix time!
    #count the p_value
  
  
  }
  
  # After completing R simulations, calculate the probabilities
  joint_probability <- sum(pval_matrix[, 1] > 0.05 & pval_matrix[, 2] < 0.05) / R
  probability_reject_0_4 <- sum(pval_matrix[, 1] < 0.05) / R
  probability_reject_0_5 <- sum(pval_matrix[, 2] < 0.05) / R
  
  # Store these probabilities in storage_matrix
  storage_matrix[k, 1] <- joint_probability
  storage_matrix[k, 2] <- probability_reject_0_4
  storage_matrix[k, 3] <- probability_reject_0_5
  
  colnames(storage_matrix) <- c("Joint Prob (Not Reject 0.4 AND Reject 0.5)",
                                "Prob Reject 0.4",
                                "Prob Reject 0.5")
  
  # Set row names based on n_value to make it more readable
  rownames(storage_matrix) <- paste("n =", n_value)
}

print(storage_matrix)










