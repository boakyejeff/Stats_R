## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Addition-------------------------------------------------------------------------------------------
1+1


## ----Multiplication-------------------------------------------------------------------------------------
5*4


## ----Division-------------------------------------------------------------------------------------------
100/5


## ----Spaces and Comments--------------------------------------------------------------------------------
1+1 # Here I am adding 1 and 1 together
1 + 1
1    +    1 # Here I am adding 1 and 1 together with spaces


## ----Commenting a line out------------------------------------------------------------------------------
1+1 # Here I am adding 1 and 1 together
# 5*4 # Do NOT multiply 5 and 4!
5*4 # Do multiply 5 and 4!


## ----Order of Operations--------------------------------------------------------------------------------
30/2+1 
30/(2+1)


## ----Incomplete line------------------------------------------------------------------------------------
30/
(2+1)

## equality is used in logic or data manipulation
#can be used for less than, greater than and more
## ----Equality-------------------------------------------------------------------------------------------
1 == 1
1 == 0
1==1



1 == 1

## ----Greater than and less than-------------------------------------------------------------------------
3 > 2
10 < 9


## ----At least and at most-------------------------------------------------------------------------------
1 >= 1
1 <= 0 
1 >= 
  1
1 <= 0

## ----Basic functions------------------------------------------------------------------------------------
sqrt(4)  #square root of 4
log(100)   # log = natural log = ln() by default!
log(100, base = 10) # log of 100 in base 10
exp(1)   # e^1
log(exp(20))

####exponential notation
exp(100)   #same as 2.688117 x 10^43


## ----Scientific Notation--------------------------------------------------------------------------------
exp(28)


## ----Scientific Notation2-------------------------------------------------------------------------------
options(scipen=999)
exp(28)
options(scipen=0)
exp(28)


## ----Assignment, error=TRUE-----------------------------------------------------------------------------
var <- 8100
var
var/2


## ----Assignment2, error=TRUE----------------------------------------------------------------------------
VaR


## ----Assignment precedence, error=TRUE------------------------------------------------------------------
w = x <- 1
y <- z = 1


## ----Assignment precedence 2, error=TRUE----------------------------------------------------------------
var_1

sum(var_1 <- 1, 1)
var_1
g <- 7 -> h
g
h
sum(var_a <- 2,3,4*8)
var_a 

## ----pipe_operator cont---------------------------------------------------------------------------------
# no piping
sqrt(log(abs(-2000), base = 10))


## ----pipe_operator cont2--------------------------------------------------------------------------------
# piping
-2000 |> abs() |> log(base = 10) |> sqrt()


## ----Vectors1, error=TRUE-------------------------------------------------------------------------------
my_vector <- c(1, 2, 3, 4, 5) # c is for "combine"
my_vector

 ## "c" combines data of the same type numbers or strings
my_vector_1 <- c(1,2,3,4,5)
my_vector_1

some_colors_1 <- c("red","green", "blue")
some_colors_1


## ----Vectors2, error=TRUE-------------------------------------------------------------------------------
some_colors <- c("red", "green", "blue")
some_colors

#if you use a  mix strings to numbers it will code all as strings example below
some_colors_11 <- c("red","green", "blue",1,3,4,5)
some_colors_11


## ----Vector_Operations, error=TRUE----------------------------------------------------------------------
my_vector
my_vector + 1
my_vector^2

#whole vectors can be operated upon.
my_vector_1+1
my_vector_1^2
my_vector_1*exp(2)
exp(2)

## ----Combining Vectors, error=TRUE----------------------------------------------------------------------
first_vec <- c(1, 2, 3)
second_vec <- 4:6 # same as c(4, 5, 6)


## ----Combining Vectors 2, error=TRUE--------------------------------------------------------------------
c(first_vec, second_vec)
c(second_vec, first_vec)
second_vec/first_vec # 4/1, 5/2, 6/3


## ----Making Sequences, error=TRUE-----------------------------------------------------------------------
seq(from = 6, to = 36, by = 3)


## ----Making Sequences by length, error=TRUE-------------------------------------------------------------
seq(from = 6, to = 36, length.out = 5)
seq(from = 6, to = 36, length.out = 20)


## ----Plotting a Function Setup, error=TRUE--------------------------------------------------------------
x_vec <- seq(from = 1, to = 1000, length.out = 25)
# Take advantage of being able to operate on an entire vector
y_vec <- log(x_vec)


## ----PlottingtheFunction, error=TRUE, fig.align="center", fig.height=2, fig.width=4---------------------
par(mar = c(2,2,2,2))
plot(x = x_vec, y = y_vec)


## ----rep function 1, error=TRUE-------------------------------------------------------------------------
rep(x = 1, times = 5)


## ----rep function 2, error=TRUE-------------------------------------------------------------------------
rep(x = c(1,2,3), times = 3) # times repeats the vector
rep(x = c(1,2,3), each = 3) # each repeats elements


## ----Index Example, error=TRUE--------------------------------------------------------------------------
x <- c("red","blue","green","purple","yellow")
x


## ----Index Example 2, error=TRUE------------------------------------------------------------------------
x[3]


## ----Index with Vectors, error=TRUE---------------------------------------------------------------------
x
x[c(1,3,5)]     # first, third, and fifth element
length(x) 
x[length(x)] # last element
x[rep(c(2,3), each = 2)] # rep(c(2,3), each = 2) = c(2,2,3,3)


## ----Removing elements, error=TRUE----------------------------------------------------------------------
x
x[-1] # removing the first element
x[-c(1,3,5)]     # removing the first, third, and fifth element
x[-length(x)] # removing the last element


## ----Replacing elements, error=TRUE---------------------------------------------------------------------
x
x[1] <- "black"
x
x[2:3] <- c("gold","pink")
x


## ----Vector_Operations_reminder, error=TRUE-------------------------------------------------------------
my_vector
my_vector + 1
my_vector^2


## ----Adding two vectors, error=TRUE---------------------------------------------------------------------
vec_1 <- c(1, 2, 3)
vec_2 <- c(4, 5, 6)
vec_1 + vec_2


## ----Single value is a vector of length 1, error=TRUE---------------------------------------------------
10


## ----Vector_recycling, error=TRUE-----------------------------------------------------------------------
x <- c(1,2,3,4,5,6)
x * c(1, -1)


## ----Vector_recycling_2, error=TRUE---------------------------------------------------------------------
x - 1
