# Network Modeling - HS 2022
# C. Stadtfeld, A. Espinosa-Rada, A. Uzaheta
# Based on previous work from: K. Mepham, V. Amati
# Social Networks Lab
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 26 September 2022
#
# Introduction to R

## R as a calculator -------------------------------------------------------
5 + 8
15^2
exp(3)
log(1)
sqrt(4)


## Logical operators -------------------------------------------------------
5 > 3
a <- 4  # assignment operator (->) works the same as (=)
b <- 6
a == b
(a == b) * 1 # FALSE is evaluated as 0
a <= b
(a <= b) * 1 # TRUE is evaluated as 1
a != b


# Type of objects in R ----------------------------------------------------

## Vectors -----------------------------------------------------------------
# Function c
vec0 <- c(2, 3, 4)
vec0
# WARNING:
# do not use c as a name of an R object!!!

Vec0
# WARNING:
# R is case sensitive!!! Therefore, Vec0 is different from vec0

# Colon operator
vec1 <- 1:3
vec1

# The seq function
vec2 <- seq(from = 2, to = 5, by = 0.5)
vec2

# The rep function
vec3 <- rep(1:3, times = 2)
vec3

# Character vector
vec4 <- c("a", "b", "c")
vec4


# Vector computation
vec0 + vec1
log(vec1)
length(vec1)
sum(vec1)
max(vec1)
min(vec1)

# The recycling rule
vec0 + vec3


# Extract elements from a vector, the subsetting operators
vec2[1]
vec2[c(1, 3)]

## Matrices ----------------------------------------------------------------
# The function matrix
mat1 <- matrix(1:10, nrow = 2, ncol = 5, byrow = FALSE)
mat1
mat2 <- matrix(1:10, nrow = 2, ncol = 5, byrow = TRUE)
mat2


# Functions rbind and cbind
mat3 <- rbind(vec0, vec1)
mat3
# or as column vectors
mat4 <- cbind(vec0, vec1)
mat4

# Number of rows and columns
dim(mat1)


# Multiplication
mat1 * mat2
# WARNING: the operator * computes the element-wise (Hadamard) product!!!
#          The operator for the matrix product is %*%
mat5 <- mat1 %*% t(mat2)
mat5

solve(mat5) # Calculates the inverse
det(mat5) # Calculates the determinant
t(mat1) # Calculates the transpose
# WARNING:
# t is the function that computes the transpose of a matrix.
# Do not use t as the name of an R object!!!


# Extract elements from matrices
mat1
mat1[1, 2] # single cell
mat1[1, ] # first raw
mat1[, 1] # first column
mat1[-1, -1] # A negative pair results in a minor matrix
# where a column and a row are omitted.


## Arrays ------------------------------------------------------------------
arr0 <- array(1:48, dim = c(4, 4, 3))
arr0
arr1 <- array(c(mat1, mat2), dim = c(2, 5, 2))
arr2 <- array(c(mat1, mat2, mat4), dim = c(2, 5, 3))


# How could you get the first matrix of arr1 using the subsetting operators?
arr1[, , 1]
# How could you get the first row of each matrix in arr1?
arr1[1, , ]
# How could you get the first column of each matrix in arr1?
arr1[, 1, ]


## Data frames -------------------------------------------------------------
# Creating data frames
peopleid <- 1:10
gender <- rep(c("Male", "Female", NA), times = c(4, 5, 1))
# data frame can also handle missing values that are usually coded as NA (not available)
set.seed(1908)
age <- sample(20:30, 10)
age
height <- rnorm(n = 10, mean = 165, sd = 5) |> ceiling()
dat1 <- data.frame(id = peopleid, gender = gender, age = age, height = height)

# Inspecting a data frame
head(dat1) # shows the first six lines
dim(dat1) # returns the number of rows and columns
names(dat1) # shows the names of the columns
summary(dat1) # summary statistics for each column

# The elements of a data.frame can be addressed as in a matrix
dat1[1, 2] # single cell
dat1[1, ] # first raw
dat1[, 1] # first column

# OBSERVATION: name of the columns can be used to address columns
dat1$id
dat1[, "id"]
# How could you extract id and age variables?
dat1[, c("id", "age")]


## Lists -------------------------------------------------------------------
list0 <- list(vec0, mat1, dat1)
str(list0)
list0[[1]]
list0[[3]][1:3, ]


### Objects structure -------------------------------------------------------
x <- mat2
is.vector(x)
is.matrix(x)
is.data.frame(x)
str(x)


### Coercion ----------------------------------------------------------------

y <- as.vector(mat2)
z <- as.matrix(y) # Vector are coerced into column matrix

u <- as.data.frame(mat2)
u

# OBSERVATION: R automatically creates column names
# when using the function as.data.frame
# To change these column names we can use the function names

names(u) <- c("a", "b", "c", "d", "e")
u


# Control flow ------------------------------------------------------------


## Conditional statements ---------------------------------------------------
# if (condition: logical value) {expression to do}

if (a  > b && b > 1) {
  print("a is bigger than b")
  c <- a + b
} else if (b > a) {
  print("a is bigger than b")
  c <- b - a
} else
  print("a is equal to b")

# the longer form of the logical operators (&&, ||) is preferred for
# programming control-flow and typically use in `if` clauses

# vectorised if
# ifelse(condition, when TRUE, when FALSE)
vec2Modified <- ifelse(vec2 > 3, vec2 + 2, vec2 * 2)

## Loops -------------------------------------------------------------------
# for (i in set of values) {expression to do}

# For example, we can use a for loop to compute the sum of the values
# in each row of the matrix mat2
for (i in 1:nrow(mat2)) {
  x <- sum(mat2[i, ])
  print(x)
}

# Other loop construction:
# while (condition) {expression}
# repeat {expression}


# Functions ---------------------------------------------------------------
# We can create a function sumrow that computes the sum of each row
# in a matrix x and return them in a vector called rowsum

sumrow <- function(x) {
  rowsum <- NULL
  for (i in 1:nrow(x)) {
    rowsum[i] <- sum(x[i, ])
  }
  return(rowsum)
}

sumrow(mat2)

# This is just an illustration
# It's better to use vectorized versions of operations
# or even the native functions

# Alternative solutions
rowSums(mat2)

# Apply family ------------------------------------------------------------
# apply a function to an array in a given dimension 
apply(X = mat2, MARGIN = 1, FUN = sum)

# apply a function to a vector by each level of a factor variable
tapply(X = dat1$age, INDEX = dat1$gender, FUN = mean)

# sum the rows of a numeric matrix-like for each level of a grouping variable
rowsum(x = dat1[, c("age", "height")], group = dat1$gender)

# apply a function to each component of a list
lapply(X = list0, FUN = typeof)
  # same with a simplify output lapply(x, f, simplify = "array")
sapply(X = list0, FUN = typeof)

# other functions of the apply family
# vapply(), replicate(), mapply()

# Access the Help files documentation -------------------------------------
# to access the documentation of a function
?matrix
?rowSums
# when don't where to start, use general terms. It search on installed packages
help.search("regression")

# Always Google your R programming questions or use https://rseek.org/
# https://adv-r.hadley.nz/  is a good source to learn.


# Working with data -------------------------------------------------------

## Setting working directory -----------------------------------------------
## getwd()
## setwd("C:/")
##
## # WARNING: The backslash character has a special meaning to R.
## # When you specify a path in the RConsole you have to either
## # double-up the backslashes or use forward slashes instead.
## # setwd("C:\\...")
##
## list.files() # Listing the files in the working directory

## If you use RStudio as the R IDE
## It sets the working directory to the project folder


## Data import and export --------------------------------------------------
# We export the data frame dat1 in a txt file comma separated
# ?write.table
write.table(dat1, "dat1.txt", sep = ",", row.names = FALSE, col.names = TRUE)

# # For tab spacing you should use  sep="\t"
#
# We import data using the function read.table.
? read.table
dataImport <- read.table("dat1.txt", sep = ",", header = TRUE)
str(dataImport)


## Saving objects to a file ------------------------------------------------
# Saving the complete workspace
save.image("Intro.RData")
load("Intro.RData")

# Saving a particular object
saveRDS(dataImport, "dataImport.RDS")
dataImport <- readRDS("dataImport.RDS")
