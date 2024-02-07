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
cat("E[Yi | X_i1 = 0 , X_i2 = 2] =", (-0.35+0.33)*(0.5))

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
  
  #all the part in the OLS equation
  a <- 0.8
  beta_hat_one <- 0
  beta_hat_two <- 0
  beta_hat_three <- 0.1
  
  #Rround
  R <- 1000
#######################################################################################
#Q3-A-1

  #Set the moving part 
  n_1 <- 100
  e_1<- rnorm(n_1, mean = 0, sd = 1)

  #generate the data
  for(i in 1:100){
    x_1 <- mvrnorm(n_1, mu,Evil_variance_matrix)
    
    y_1 <- a + 
  }
  
  
  # set matrix for storage, 1000 obs in test set
  pred <- matrix(rep(NA,144),48,3)
  # loop
  for(i in 1:48){
    tmp0 <- 1970
    tmp1 <- n.end+(i-1)*1/4
    tmp <- window(rgdp.gr,tmp0,tmp1)
    pred[i,1] <- window(rgdp.gr,tmp1+1/4,tmp1+1/4) # actual
    # compute forecasts
    pred[i,2] <- forecast(Arima(tmp,order=c(1,0,0)),h=1)$mean # AR(1)
    pred[i,3] <- forecast(Arima(tmp,order=c(0,0,1)),h=1)$mean # MA(1)
  }

  
  #calcaute the y!
  y_1 <- a + x_1
  
  

#######################################################################################
  
  
for the first samples zie
cosndier the n = 100 
mnr nrom functi0n 
we will createt he r = 1000 different drawas from the mutivariance normal,w ewill 100 data set, fror every 100 data set we
will go test 20 nonal-hypothesis, at the confidence level of 0.05

c <- c(seq(from = -1, to =1, by = 0.1))
ou will find the 
put it the 100 sample sence into the sampel set, y

for loop proceture, whenever you refgerence the simple size, when you giggther order when you are doing forh t esample, you can put the 

if we define the vetctor n = c(100, 250, 1000)






## For the question 3

N c(100, 250, 1000)
for(: n for N): 
  
    
  
n for N, that is sourt if you refer to the vecto if will 




for R,
#######################################################################################
3-c
(i) the T test
(ii)the F test


#######################################################################################




