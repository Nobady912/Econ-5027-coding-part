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

#doctor who blue 
dw_blue = "#003b6f"
#Carlton red
cr_red = "#e91c25"
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
#Q3

#Cleaning the environment
rm(list = ls())
dev.off()

# set the enviroment(things that will not moving about)
  #variance matrix
  Evil_variance_matrix <- matrix(c( 1.0964, -0.5313, -0.5730, 
                                  -0.5313, 0.9381, -0.4184,
                                  -0.5730, -0.4184, 1.0228), nrow = 3, ncol = 3, byrow = TRUE)
  #vector of means
  mu <- c( 1.1141,-0.6768,3.3521)
  
  #the c
  c <- c(seq(from = -1, to =1, by = 0.1))
  
  #all the part in the OLS equation
  a <- 0.8
  beta_hat_one <- 0
  beta_hat_two <- 0
  beta_hat_three <- 0.1
  
  #nest loop r
  
  #Rround
  R <- 1000
#######################################################################################
#Q3-A-1
  
# Parameters
n_values <- c(100, 250, 500, 1000)
c_values <- seq(from = -1, to = 1, by = 0.1)
R <- 1000
beta_true <- c(0.8, 0, 0, 0.1)  # True coefficients

# Initialize a matrix to store the proportions of rejections
proportion_reject <- matrix(0, nrow = length(n_values), ncol = length(c_values))
rownames(proportion_reject) <- n_values

# Run simulations
for (n_idx in 1:length(n_values)) {
  n <- n_values[n_idx]
  
  for (c_idx in 1:length(c_values)) {
    c <- c_values[c_idx]
    rejections <- 0
    
    for (i in 1:R) {
      # Simulate the data as in previous code ...
      # Calculate residuals, t-value, and p-value as in previous code ...
      
      # Record if the null is rejected
      if (p_value < 0.05) {
        rejections <- rejections + 1
      }
    }
    
    # Store the proportion of rejections
    proportion_reject[n_idx, c_idx] <- rejections / R
  }
}

# Plot the power curves using R's base graphics
colors <- rainbow(length(n_values))
plot(c_values, proportion_reject[1,], type = "l", col = colors[1], ylim = c(0, 1),
     xlab = "Value of c", ylab = "Proportion of Null Rejections",
     main = "Power Curves for Different Sample Sizes")
for (i in 2:length(n_values)) {
  lines(c_values, proportion_reject[i,], col = colors[i])
}
legend("topright", legend = as.character(n_values), col = colors, lty = 1)


    
    #####

# Parameters
n_values <- c(100, 250, 500, 1000)
c_values <- seq(from = -1, to = 1, by = 0.1)
R <- 1000
alpha <- 0.05
beta_true <- c(0.8, 0, 0, 0.1)  # True coefficients

# Placeholder for results
results <- matrix(0, nrow = length(c_values), ncol = length(n_values))

# Simulations
for (n_idx in seq_along(n_values)) {
  n <- n_values[n_idx]
  rejections <- rep(0, length(c_values))
  
  for (r in 1:R) {
    x <- mvrnorm(n, mu = mu, Sigma = Evil_variance_matrix)
    x <- cbind(rep(1, n), x)  # Add intercept
    e <- rnorm(n, mean = 0, sd = 1)  # Error term
    y <- x %*% beta_true + e  # Response variable
    
    # OLS estimation
    fit <- lm(y ~ x - 1)  # -1 to exclude automatic intercept
    beta_hat <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
    
    # Hypothesis tests for each c
    for (c_idx in seq_along(c_values)) {
      c <- c_values[c_idx]
      t_stat <- (beta_hat[3] - c) / se[3]
      p_val <- 2 * (1 - pt(abs(t_stat), df = n - length(beta_hat)))
      if (p_val < alpha) {
        rejections[c_idx] <- rejections[c_idx] + 1
      }
    }
  }
  # Store the proportion of rejections
  results[, n_idx] <- rejections / R
}

# Plotting
# Colors
dw_blue <- "#003b6f"  # Doctor Who blue
cr_red <- "#e91c25"   # Carlton red
ua_green <- "#007c41"  #ualberta green 
ua_gold <- "#ffdb05"  #ualberta gold
colors <- c(dw_blue, cr_red, ua_green ,ua_gold)  # Assign colors to sample sizes

# Base R Plotting with specified colors
plot(NULL, xlim = c(min(c_values), max(c_values)), ylim = c(0, 1),
     xlab = "Value of c", ylab = "Proportion of Rejections",
     main = "Power Curves for Different Sample Sizes")
for (n_idx in seq_along(n_values)) {
  lines(c_values, results[, n_idx], type = "l", lwd = 2, col = colors[n_idx])
}
legend("bottomright", legend = paste("n =", n_values), col = colors, lty = 1, cex = 0.8)

    
#doctor who blue 
dw_blue = "#003b6f"
#Carlton red
cr_red = "#e91c25"
    
    
    

