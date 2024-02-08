#################################################################################################################
#Tie Ma
#student number: 101316917
#ECON 5027
#Assignment


#log 

#v0.1: finished the question 2 and now doing the equation 3 2024/02/07
#################################################################################################################
#Clean the environment
rm(list = ls())
dev.off()

#set the direction
setwd("~/Documents/GitHub/Econ-5027-coding-part")

#Lode the all package that necessary.
#some of the packages may not been used in this homework.
#I just copy it around
library("MASS")
library("ggplot2")

# Colors
dw_blue <- "#003b6f"  # Doctor Who blue
cr_red <- "#e91c25"   # Carlton red
ua_green <- "#007c41"  #ualberta green 
ua_gold <- "#ffdb05"  #ualberta gold
#################################################################################################################
#Q2
######################################################################################
#The data time! 

#store the data
data <- c(0.23, -0.39, -0.03, 0.29, 0.33, -0.37, -0.32, -0.04, -0.51, -0.96, # Y_i
          1, 0, 1, 0, 0, 1, 0, 0, 1, 1, # X_i1
          1, 2, 1, 1, 2, 2, 3, 3, 2, 3) # X_i2
Q2_matrix <- matrix(data, nrow = 10, ncol = 3, byrow = FALSE)

#set the matrix name
colnames(Q2_matrix) <- c('Y_i', 'X_i1', 'X_i2')

#check the matrix
#print(Q2_matrix) #its look good.
######################################################################################
#Q2-1  Using the data in Table 1, compute the (sample analog of the) conditional expectation
#function E[Y i | X i1 = x 1 , X i2 = x 2 ] at all values x 1 ∈ { 0, 1 } and x 2 ∈ { 1, 2, 3 } .

#E[Yi | X_i1 = x1 , X_i2 = x2]

#E[Yi | X_i1 = 0 , X_i2 = 1]
print("E[Yi | X_i1 = 0 , X_i2 = 1] = 0.29*0.1")

#E[Yi | X_i1 = 0 , X_i2 = 2]
cat("E[Yi | X_i1 = 0 , X_i2 = 2] =", (-0.35+0.33)*(0.2))

#E[Yi | X_i1 = 0 , X_i2 = 3]
cat("E[Yi | X_i1 = 0 , X_i2 = 3] =", (-0.32 - 0.04)*(0.2))

#E[Yi | X_i1 = 1 , X_i2 = 1]
cat("E[Yi | X_i1 = 1 , X_i2 = 1] =", (0.23 -0.03)*(0.2))

#E[Yi | X_i1 = 1 , X_i2 = 2]
cat("E[Yi | X_i1 = 1 , X_i2 = 2] =", (-0.37-0.51)*(0.2))

#E[Yi | X_i1 = 1 , X_i2 = 3]
cat("E[Yi | X_i1 = 1 , X_i2 = 3] =", -0.96*0.1)

#creat matrix for the q2-b
evil <- c(0.029,-, -0.18, 0.1, -0.44,-0.96)
evil_matrix <- matrix(evil, ncol = 1, byrow = FALSE)
#######

#Q2-b) Using the data in Table 1, compute the (sample analog of the) expectation E[Y i], a
#nd show that E[Y i ] = E[E[Y i | X i1 = x 1 , X i2 = x 2 ]] (that is, numerically verify the law of iterated expectations).

#calcaute the E[Yi]
mean_y_i <- mean(Q2_matrix[, 1])
cat("E[Yi] =",mean_y_i)


#calculate the E[E[Yi | X_i1 = x1 , X_i2 = x2]]
cat( "E[E[Yi | X_i1 = x1 , X_i2 = x2]] =", mean(evil))

#######################################################################################
#Q3-A

# Assuming you have the MASS package installed and loaded for mvrnorm
library(MASS)

# Clean the environment
rm(list = ls())

# Set the static components of the environment
Evil_variance_matrix <- matrix(c(1.0964, -0.5313, -0.5730, 
                                 -0.5313, 0.9381, -0.4184,
                                 -0.5730, -0.4184, 1.0228), nrow = 3, ncol = 3, byrow = TRUE)

mu <- c(1.1141, -0.6768, 3.3521)

# c sequence
c_value <- seq(from = -1, to = 1, by = 0.1)

# Number of simulation rounds
R <- 1000

# Parameters
n_value <- c(100, 250, 500, 1000)
beta_true <- c(-0.8, 0, 0, 0.1)  
alpha <- 0.05

# Pred matrix
proportion_reject <- matrix(0, nrow = length(n_value), ncol = length(c_value))
rownames(proportion_reject) <- n_value

# the result matrix
results <- matrix(0, nrow = length(c_value), ncol = length(n_value))

# Sim time!
for (i in 1:length(n_value)) {
  
  #set the n value in the data generate process
  n <- n_value[i]
  #track the null hypothesis be reject
  rejections <- rep(0, length(c_value))
  
  #this loop run 1000 times with current value of n
  for (j in 1:R) {
    x <- mvrnorm(n, mu = mu, Sigma = Evil_variance_matrix)
    # Add intercept (we now turn the x from 3x3 matrix to 3x4 matrix)
    x <- cbind(rep(1, n), x)  
    # Error term (standard distributed)
    e <- rnorm(n, mean = 0, sd = 1)
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
    se <- sqrt(diag(v_beta_hat))
    
    # The T-test
    for (k in 1:length(c_value)) {
      c_evil <- c_value[k]
      t_stat <- (beta_hat[3] - c_evil) / se[3]
      p_val <- 2 * (1 - pt(abs(t_stat), df = dof))
      if (p_val < alpha) {
        rejections[k] <- rejections[k] + 1
      }
    }
  }
  
  # Store the proportion of rejections
  results[, i] <- rejections / R
}


# Define custom colors
dw_blue <- "#003b6f"  # Doctor Who blue
cr_red <- "#e91c25"   # Carlton red
ua_green <- "#007c41"  # UAlberta green
ua_gold <- "#ffdb05"  # UAlberta gold
colors <- c(dw_blue, cr_red, ua_green, ua_gold)  # Assign colors to sample sizes

# Base R Plotting with specified colors
plot(NULL, xlim = c(min(c_value), max(c_value)), ylim = c(0, 1),
     xlab = "Value of c", ylab = "Proportion of Rejections",
     main = "Power Curves for Different Sample Sizes",
     xaxt = "n", yaxt = "n", bty = "n", xaxs = "i", yaxs = "i")
axis(1, at = seq(min(c_value), max(c_value), by = 0.2), las = 1, cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)

# Plotting lines using a for loop in your style
for (i in 1:length(n_value)) {
  lines(c_value, results[, i], type = "l", lwd = 2, col = colors[i], lty = 1)
}

# Dynamic legend based on the number of n_values
legendText <- paste("n =", n_value)

#legend time 
legend("top", legend = paste("n =", n_value), col = colors[1:length(n_value)], lty = 1, cex = 0.8)
title(main = "Power Curves for Different Sample Sizes", cex.main = 1.2)
title(xlab = "Value of c", cex.lab = 1)
title(ylab = "Proportion of Rejections", cex.lab = 1)


  
  
  
  
