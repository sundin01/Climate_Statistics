###############################################################################
# Exercise 1

# R interpret a vector as an 1-dimensional object (here: it is an array
# A vector is a sequence which contains only components of the same type.
a <- c(2:4)
b <- c(-1, 3, 0)

# If we define a matrix we can solve the exercise as well. But R will create
# a 2-dimensional object (is is also an array).
# We create a 1 x 3 matrix (which is basically a row-vector):
a.row <- matrix(c(2:4), nrow = 1, ncol = 3)
b.row <- matrix(c(-1, 3, 0), nrow = 1, ncol = 3)

# We create a 3 x 1 matrix (which is basically a column-vector):
a.column <- matrix(c(2:4), nrow = 3, ncol = 1)
b.column <- matrix(c(-1, 3, 0), nrow = 3, ncol = 1)

###############################################################################
# Exercise 2
# If we transpose a vector, the result will always be a matrix. Therefor, if
# we transpose a transposed vector, we will always receive a matrix as well.

# We transpose a vector (sequence) --> and receive a 1x3 matrix
a.trans <- t(a)
b.trans <- t(b)

# If we transpose a transposed vector, we receive a 3x1 matrix
a.double.trans <- t(t(a))
b.double.trans <- t(t(b))

# Here we can see that there are the same
a.double.trans == a.column
b.double.trans == b.column

###############################################################################
# Exercise 3
# we can sum two vectors very easy. R sums the vectors component-wise
c <- a + b

# We also can multiply two vectors (Hadamard product). Careful, it is neither the
# dot product nor the cross product!
d <- a * b

# There is a difference between "*" (component wise --> return is a matrix) and
# "%*%" (matrix multiplication --> not component wise --> return is a matrix)
component.wise.multi <- t(a)*b
matrix.multi <- t(a)%*%b

###############################################################################
# Exercise 4
# For this task we use indices. We write all element we want in the brackets.
a[1:2]+b[2:3]

###############################################################################
# Exercise 5

# First, we define the function:
exercise_1_function <- function(x){
  temporary_value <- sqrt(x^2+2)
  return(temporary_value)
}

# Second, we evaluate for -2, 0, and 7 --> is not a problem at all
exercise_1_function(-2)
exercise_1_function(0)
exercise_1_function(7)

# Third, we evaluate for the vector a --> is not a problem et all.
# It simply evaluates each element
exercise_1_function(a)

###############################################################################
# Exercise 6
# First, we define a new vector
tft <- c(TRUE, FALSE, TRUE)

# Second, we evaluate tft in our function from exercise 5
exercise_1_function(tft)

# Why can we do that? Because it is binary: TRUE = 1, FALSE = 0
exercise_1_function(1) == exercise_1_function(TRUE)
exercise_1_function(0) == exercise_1_function(FALSE)

