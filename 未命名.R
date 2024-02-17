library("MASS")
library("sandwich")

# No need to setwd in this context, ensure you're running it in the right directory if needed

# Sample size
n <- 10000

# Vector of means
mu <- c(2, 1, -2)

# Covariance matrix
sigma <- matrix(c(2, 1, -0.5,
                  1, 2.5, -0.25,
                  -0.5, -0.25, 1),
                nrow = 3,
                ncol = 3,
                byrow = TRUE)

# Draw data
x <- mvrnorm(n, mu, sigma)

# Add a column vector of ones to x for the constant
x <- cbind(rep(1, n), x)

# True beta (assuming some values as it was not provided)
beta_true <- c(0.5, 1.5, -1, 0.3)

# Produce a heteroskedastic error term
heteroskedastic_error <- rnorm(n, mean = 0, sd = sqrt(1:n) * 1.7)

# Generate Y
Y <- x %*% beta_true + heteroskedastic_error

# Plotting (your plotting commands seem fine, just ensure graphical parameters are set if needed)

# Part two: Produce heteroskedastic robust estimate of var

# Compute beta_hat
beta_hat <- solve(t(x) %*% x) %*% t(x) %*% Y

# Compute residuals
eps_hat <- Y - x %*% beta_hat

# Compute heteroskedastic robust errors
vcov_hc <- vcovHC(lm(Y ~ x[, -1]), type = "HC0") # Assuming x[, -1] to exclude the intercept added manually

# If you need to manually calculate, ensure correct matrix operations (your manual calculation had errors)

# Use sandwich package directly for robust errors
model <- lm(Y ~ x[, -1])
robust_var_sandwich <- diag(vcovHC(model, type = "HC0"))

# Comparing residuals
identical(round(residuals(model), 9), round(eps_hat, 9))

# Part three (bootstrapping) was not elaborated, so ensure to specify what you need here.
