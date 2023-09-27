###############################################################################

# Statistical Methods for Climate Sciences: Week 2, Patrick Bigler, 20-100-178

###############################################################################
# Exercise 1

# Let A = (10x7), B = (10x3), C = (4x7), d = (3x1),
# e = (7x1), f = (10x1) be matrices. There are 32 combinations to a
# pairwise matrix multiplication:

# A (10x7) * e (7x1) = M (10x1)
# A (10x7) * t(C) (7x4) = M (10x4)
# A (10x7) * t(A) (7x10) = M (10x10)
# t(A) (7x10) * B (10x3) = M (7x3)
# t(A) (7x10) * f (10x1) = M (7x1)
# t(A) (7x10) * A (10x7) = M (7x7)

# B (10x3) * d (3x1) = M (10x1)
# B (10x3) * t(B) (3x10) = M (10x10)
# t(B) (3x10) * A (10x7) = M (3x7)
# t(B) (3x10) * f (10x1) = M (3x1)
# t(B) (3x10) * B (10x3) = M (3x3)

# C (4x7) * E (7x1) = M (4x1)
# C (4x7) * t(A) (7x10) = M (4x10)
# C (4x7) * t(C) (7x4) = M (4x4)
# t(C) (7x4) * C (4x7) = M (7x7)

# d (3x1) * t(e) (1x7) = M (3x7)
# d (3x1) * t(f) (1x10) = M (3x10)
# d (3x1) * t(d) (1x3) = M (3x3)
# t(d) (1x3) * t(B) (3x10) = M (1x10)
# t(d) (1x3) * d (3x1) = M (1x1)

# e (7x1) * t(f) (1x10) = M (7x10)
# e (7x1) * t(d) (1x3) = M (7x3)
# e (7x1) * t(e) (1x7) = M (7x7)
# t(e) (1x7) * t(C) (7x4) = M (1x4)
# t(e) (1x7) * t(A) (7x10) = M (1x10)
# t(e) (1x7) * e (7x1) = M (1x1)

# f (10x1) * t(d) (1x3) = M (10x3)
# f (10x1) * t(e) (1x7) = M (10x7)
# f (10x1) * t(f) (1x10) = M (10x10)
# t(f) (1x10) * A (10x7) = M (1x7)
# t(f) (1x10) * B (10x3) = M (1x3)
# t(f) (1x10) * f (10x1) = M (1x1)

###############################################################################
# Exercise 2

# First, we define the 3x3 matrix as followed (fill values column wise):
A <- matrix(c(4, -1, 3, 7, 6, -3, 3, 8, 2), nrow = 3, ncol = 3)
# There are many ways to determine the determinant of a matrix.
# Up to 3x3 we can use the rule of Sarrus or we can use the Laplace expansion,
# which is a more general approach.
# But for matrices up to 3x3 it is easier to apply the rule of Sarrus, because
# the determinant of a 2x2 matrix is easy to calculate:

# We define a starting point (for example a_ij) and multiplying it with the
# determinant of the 2x2 minor matrix, which does not contain any element in
# the i-th row or the j-th column. After that, we chose another element which
# does not lie on the same row or columns of the starting point. We multiply
# it with the determinate of the next minor matrix (same conditions like before).
# After that, we do that for the last point under the same conditions again.

det_A <- 4*(6*2) - 4*(-3*8) + 7*(8*3) - 7*(-1*2) + 3*(-3*-1) - 3*(6*3)
det_A
# We proof our result with R and we see that our approach is true...
det(A)

###############################################################################
# Exercise 3

# First, we define the 3x3 matrix as followed (fill values column wise):
A <- matrix(c(2, 7, 9, 8, 3, 0, 5, 6, 5, 1, 8, 6), nrow = 3, ncol = 4)

B <- matrix(c(4, 6, 6, 2, 7, 8, 1, 4, 0, 6, 0, 1), nrow = 3, ncol = 4)

# 1. task:
A+B # is a 3x4 matrix
t(A + B) # is a 4x3 matrix (and the same as t(A)+t(B))
t(A) + t(B) # is a 4x3 matrix (and the same as t(A+B))

# We can see that this is the same for - operator
A - B # is a 3x4 matrix
t(A - B) # is a 4x3 matrix (and the same as t(A)-t(B))
t(A) - t(B) # is a 4x3 matrix (and the same as t(A-B))

# We can see this is not hold for element wise multiplication
A * B # is a 3x4 matrix
t(A * B) # is a 4x3 matrix (and the same as t(A)*t(B))
t(A) * t(B) # is a 4x3 matrix (and the same as t(A*B))

# If we divide, we must be careful because we have a zero in our matrix
A / B # is a 3x4 matrix
t(A / B) # is a 4x3 matrix (and the same as t(A)/t(B))
t(A) / t(B) # is a 4x3 matrix (and the same as t(A/B))

# I notice --> t(A+B) = t(A) + t(B) for all element wise operations

# 2. task:
# Calculate the matrix product. We can see, that the dimension is not the same!
# and they are now square matrices (but with different dimensions)
C <- A %*% t(B)

# Calculate the matrix product
D <- t(A) %*% B

# We can see, the dimension of the matrix products are not equal!
dim(C) == dim(D)

###############################################################################
# Exercise 4

# The trace of a matrix is defined for nxn matrices only.
# We can write a very simple function with sum(diag(matrix)) but this isn't cool.
# This function is more interactive and a general approach...

myTrace <- function (matrix){
  # Is it a matrix?
  if(is.matrix(matrix)){
    k <- NULL
    temp <- NULL
    numbers_of_rows <- nrow(matrix)
    numbers_of_column <- ncol(matrix)
    # Is it a nxn matrix?
    if(numbers_of_rows == numbers_of_column){
      # iterate over all rows
      for (i in 1:nrow(matrix))
        # iterate over all columns
        for (j in 1:ncol(matrix))
          # We want only the elements on the main diagonal
          if(i == j){
            # we create a vector with all main diagonal values
            k = c(k,matrix[i,j])}
      else{temp <- temp}}
    else {message('Your input is not a n x n matrix!')}
    # sum up all values on the vector
    trace <- sum(k)}
  else{message('Your input is not a matrix!')}
return(trace)}

# Second, we calculate the trace of C (defined above)
myTrace(C)

# Third, we define new matrices and calculate the trace of it...
# First, we define the 3x3 matrix as followed (fill values column wise):
A <- matrix(c(2, 8, 3,6), nrow = 2, ncol = 2)
B <- matrix(c(4, 5, 6, 4), nrow = 2, ncol = 2)
C = A + B

# We can see, that the results are the same...
myTrace(C) == (myTrace(A) + myTrace(B))
