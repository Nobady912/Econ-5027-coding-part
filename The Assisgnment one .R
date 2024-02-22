#################################################################################################################
#Tie Ma
#student number: 101316917
#ECON 5027
#Assignment one 
#################################################################################################################



#################################################################################################################
#Clean the environment
rm(list = ls())

#set the direction
setwd("~/Documents/GitHub/Econ-5027-coding-part")

#Lode the all package that necessary.
#some of the packages may not been used in this homework.
#I just copy it around
library("MASS")
library("ggplot2")
##############################

##Q2-D########################


#loding the data
Q2D_matrix <- matrix(c(
  0.23, -0.39, -0.03, 0.29, 0.33, -0.37, -0.32, -0.04, -0.51, -0.96,  # Yi
  1, 0, 1, 0, 0, 1, 0, 0, 1, 1,                                       # Xi1
  1, 2, 1, 1, 2, 2, 3, 3, 2, 3                                        # Xi2
),
nrow=10,  
ncol=3, 
byrow=FALSE )

#add one for the intercept
x <- cbind(1, Q2D_matrix[, 2:3]) 
y <- Q2D_matrix[, 1]

#time for the beta_hat
beta_hat <- solve(t(x) %*% x) %*% (t(x) %*% y)

#constructed the model
Q2D_model <- function(x1, x2, beta_hat) {
  beta_hat[1] + beta_hat[2] * x1 + beta_hat[3] * x2
}

#lode the data
Xi1_values <- c(0, 1)
Xi2_values <- c(1, 2, 3)

#The data frame
Q2D_data_frame <- expand.grid(Xi1 = Xi1_values, Xi2 = Xi2_values)

# Calculate the conditional expectations everything
conditional_expectations <- c(
  beta_hat[1] + beta_hat[2] * 0 + beta_hat[3] * 1,
  beta_hat[1] + beta_hat[2] * 0 + beta_hat[3] * 2,
  beta_hat[1] + beta_hat[2] * 0 + beta_hat[3] * 3,
  beta_hat[1] + beta_hat[2] * 1 + beta_hat[3] * 1,
  beta_hat[1] + beta_hat[2] * 1 + beta_hat[3] * 2,
  beta_hat[1] + beta_hat[2] * 1 + beta_hat[3] * 3
)

# Create a matrix for the calculated values and the corresponding Xi1 and Xi2 values
conditional_matrix <- cbind(
  "E[Yi|Xi1=x1,Xi2=x2]" = conditional_expectations,
  Xi1 = c(0, 0, 0, 1, 1, 1),
  Xi2 = c(1, 2, 3, 1, 2, 3)
)

# Print the matrix with 5 decimal places
print(conditional_matrix)


#e) 
#it the same 


##############################

#Q3-A##h0 = beta_2 = c versu h1 beta_2 not equal to c######################################################################################################

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

#set up the beta_ture
beta_true <- c(-0.8, 0, 0, 0.1) 

#the t-test with 0.05 degree of freedom (do not change to a you will lost it)
alpha <- 0.05



# The storage matrix
  #final_matrix
    Final_matrix <- matrix(0,ncol = length(c_value), nrow = length(n_value))

#the simulation time


for (i in 1:length(n_value)){
  #rest the n
  n <- n_value[i]
  #reset the TF matrix
  TF_matrix <- matrix(NA, ncol=length(c_value), nrow = R)
  
  #this loop run 1000 times with current value of n
  for (j in 1:R) {
    
    x <- mvrnorm(n, mu = mu, Sigma = evil_variance_matrix) # its should be "Sigma" not "sigma"
    
    # Add intercept (we now turn the x from 3x3 matrix to 3x4 matrix)
    x <- cbind(rep(1, n), x)
    # e need to be processed within the loop 
    e <- rnorm(n, mean = 0, sd = 1)  #normal distribution 
    #now its the time for the calculate the y!
    y <- x %*% beta_true + e
    
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
    for (k in 1:length(c_value)) {
      #T statistic for the beta3
      t_st <- (beta_hat[3,] - c_value[k])/sd[3]
      #T test P value 
      t_p_value <- 1 - pt(abs(t_st), dof) #using the TA's note!
   #the matrix time!
      #count the p_value
      TF_matrix[j, k] <- t_p_value < 0.05
      #this things was crashed my mac for times。
      #I spend 2 hours on this line a lone
     }
   }
  Final_matrix[i, ] <- colMeans(TF_matrix)
  #and other hour on this line.
 }

#god tis finally end, I rest in peace.

    
    #change the row name of the matrix 
    rownames(Final_matrix) <- c("n = 100", "n = 250", "n = 500", "n = 1000")
    

    # Assuming Final_matrix and c_value are already defined
    
    # Colors
    dw_blue <- "#003b6f"  # Doctor Who blue
    cr_red <- "#e91c25"   # Carlton red
    ua_green <- "#007c41"  # ualberta green 
    ua_gold <- "#ffdb05"  # ualberta gold
    
    # Prepare the data
    n_labels <- c("n = 100", "n = 250", "n = 500", "n = 1000")
    colors <- c(dw_blue, cr_red, ua_green, ua_gold)
    
    # Plotting
    plot(c_value, Final_matrix[1, ], type = "l", col = colors[1], ylim = c(min(Final_matrix), max(Final_matrix)), xlab = "Value of c", ylab = "Proportion of Rejections", main = "Power Curves for Different Sample Sizes", lwd = 2, xlim = c(-1, 1))
    lines(c_value, Final_matrix[2, ], col = colors[2], lwd = 2)
    lines(c_value, Final_matrix[3, ], col = colors[3], lwd = 2)
    lines(c_value, Final_matrix[4, ], col = colors[4], lwd = 2)
    legend("top", legend = n_labels, col = colors, lty = 1, lwd = 2)

##############################################################

#Q3-B########################################################
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
    alpha <- 0.05
    
    #final_matrix
    Final_matrix <- matrix(0,ncol = length(c_value), nrow = length(n_value))
    
    #the simulation time
    for (i in 1:length(n_value)){
      #rest the n
      n <- n_value[i]
      #reset the TF matrix
      TF_matrix <- matrix(NA, ncol=length(c_value), nrow = R)
      
      #this loop run 1000 times with current value of n
      for (j in 1:R) {
        
        #take the section of the data from the DGP and to do the next set.
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
        
        # e need to be processed within the loop 
        p <- 3
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
        for (k in 1:length(c_value)) {
          #T statistic for the beta2
          t_st <- (beta_hat[2,] - c_value[k])/sd[2]
          #T test P value 
          t_p_value <- 1 - pt(abs(t_st), dof) #using the TA's note!
          #the matrix time!
          #count the p_value
          TF_matrix[j, k] <- t_p_value < 0.05
          #this things was crashed my mac for times。
          #I spend 2 hours on this line a lone
        }
      }
      Final_matrix[i, ] <- colMeans(TF_matrix)
      #and other hour on this line.
    }
    
    #change the row name of the matrix 
    rownames(Final_matrix) <- c("n = 100", "n = 250", "n = 500", "n = 1000")
    
    
    # Assuming Final_matrix and c_value are already defined
    
    # Colors
    dw_blue <- "#003b6f"  # Doctor Who blue
    cr_red <- "#e91c25"   # Carlton red
    ua_green <- "#007c41"  # ualberta green 
    ua_gold <- "#ffdb05"  # ualberta gold
    
    # Prepare the data
    n_labels <- c("n = 100", "n = 250", "n = 500", "n = 1000")
    colors <- c(dw_blue, cr_red, ua_green, ua_gold)
    
    # Plotting
    plot(c_value, Final_matrix[1, ], type = "l", col = colors[1], ylim = c(min(Final_matrix), max(Final_matrix)), xlab = "Value of c", ylab = "proportion of rejections", main = "3-B power curves for different sample size", lwd = 2, xlim = c(-1, 1))
    lines(c_value, Final_matrix[2, ], col = colors[2], lwd = 2)
    lines(c_value, Final_matrix[3, ], col = colors[3], lwd = 2)
    lines(c_value, Final_matrix[4, ], col = colors[4], lwd = 2)
    legend("bottomleft", legend = n_labels, col = colors, lty = 1, lwd = 2)
    
##### Q3-c#####################################################
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
    
    
##################
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    