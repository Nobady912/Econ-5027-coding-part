#This R file is for the test the matrix loop

rm(list = ls())
#set the test enviroment
n <- 100
R <- 100000

#the count matrix
count_matrix <- numeric(R)
#the compare number
c_value <- seq(from = -1, to = 1, by = 0.1)


for(i in 1:R){
  random_number <- rnorm(n)
  
  for(j in 1:length(c_value)){
    test_matrix <- random_number < c_value[j]
    number_test_matrix <- as.numeric(test_matrix)
    count_matrix[i] <- sum (number_test_matrix)
  }
  }


random_number <- rnorm(n)
test_matrix <- random_number < 1
number_test_matrix <- as.numeric(test_matrix)