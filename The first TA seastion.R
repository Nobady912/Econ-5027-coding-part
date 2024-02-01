# for future Tie this is the review


## part two: basic comamnds, getting help, operators, and style. 

  #best practices
    rm(list = ls()) #clean the enviroment 
    
    x <- 5
    x1 <- 1
    x2 <- 4
    y <- 3
    #is()
    rm(list = "x")
      #rm the list "x"?
      #only remove the name of the 
    
  #one of the most useful features in R, the ? operator
    
  
#we should using only this method for the materix!  
example_matrix <- matrix (c (2,2,
                             1,1), nrow = 2, ncol = 2 ) #row横着的 #col竖着的


rm(list = is())

# neq operator
 5 !=5
 #在R中，5 != 5 是一个比较表达式，用于检查!=运算符左侧的值是否不等于右侧的值。
 
# i
  cat("1 + 1 = ", 1 + 1)
  
#ii
  cat("5*7=", 5 * 7)
  
#iii
  cat("10/2=", 10/2)
  
#iv
  cat("5^2 = ", 5**5)
  
#v
  cat('5^2=', 5^2) 
  

#storing values and performing operations
    a <- 1
    b <- 1
    
cat("a+b=", c+ b)
 
  A <- 2
  B <- 1
  a <- 4
  b <- 6

cat("a*b=", c *b)

cat("A/B = ", A/B)
cat("a/b = ", a/b)

#access variable from the console
#variables are also visible in the "enviroment tab"

####PART FOUR: Vector and matrices

#the area for define
  v1 <- c(2,4,3,6, 3, 4, 2)
  
# this is the weird note?
    #he put here to warning people that you should using matrix command do it? 
    #hard to understand
    # Tie 2024/01/31


#A note on row/col vector
  nrow(v1)
  ncol(v1)

# ementwise Operations: In R, when you perform operations
 #(like multiplication) on vectors, it treats them as elementwise.
 # For example, if v1 and v2 are two vectors, v1*v2 will multiply 
 # each corresponding element of v1 and v2.
# Inner Product: 
    #The inner product (or dot product) is a different operation. 
    #In other programming languages, v1*v2 might represent the inner product. However, in R,
    # you calculate it using a different approach, typically %*%. So, v1 %*% v2 would give the inner product.
# Transpose of a Vector: The function t() in R is used for transposing. Transposing a column vector turns 
    #it into a row vector, and vice versa. In R, vectors are generally 
    #considered as column vectors. When you apply t() to a vector, it treats it 
    # as a row vector. The functions nrow() and ncol() can be used to check th
    #e number of rows and columns.

# ok, I guess his point here is 
      # * is for the elementwise operations
      # %*% is for the innner product. 

# The function t() is used to take the transpose of a vector? 
    # t(v) transfer v to row
    # t(t(V)) trasfer v to col
    #row横着的 #col竖着的
  
  
# the outer product? 

M2 <- matrix(c(
  4,-1,6,
  -9, 3, 6,
  -4, 5, -2
  ), nrow = 3, ncol = 3, byrow = TRUE)

#Data: The first argument is the data vector. If the data vector isn't long enough 
    #to fill the matrix, it will be recycled. If the data vector is too long, 
    #the excess elements will be discarded.
#nrow: This parameter specifies the number of rows in the matrix.
#ncol: This parameter specifies the number of columns in the matrix.
#byrow: This logical parameter determines whether the matrix is filled by rows. 
    #If TRUE, the matrix is filled by rows; if FALSE (the default), 
    #the matrix is filled by columns.
#dimnames: This optional parameter is a list of two character vectors: the row and column names.

M1 <- matrix(c(
  2,4,2,
  5,2,1,
  2,3,4,
  5,7,8,
  5,3,2
  ), nrow = 5, ncol = 3, byrow = TRUE)

#there should not comma in the last number in the matrix

# M1 * M2
#Error in M1 * M2 : non-conformable arrays
  #M1 %*% M2
    #%*% using this for the matrix calcuation!

##some anooying the matrix note!

#generate a vector of zeros with 10 elements
  z1 = rep(0,10)
    #better explanation
      #repeat zero 10 times, 

 #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/rep

  z2 = rep(1,10)
    #repeat 10 times 1
  
  z3 = rep(NA, 8)
    #generate NA (empty) for 8 times.
  

#PART FIVE: Random Number generation#####
   # R can also be used to generat a vector of random numbers. 
  
  # I do not know why I copy the same as the TA's note
  # just too sleep and run out of brain energy. 
  
  u1 - runif()
    #Uniform: The Uniform Distribution
    #what is the fuck is the uyniform dfistribution? 
  
  
  
  
  
  
  

