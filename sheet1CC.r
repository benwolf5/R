################################################################################
# 1.1
################################################################################

################################################################################
# a)

b<-c(1,1,-1)
A<-matrix(c(3,1,4,2,0,0,1,3,4), nrow=3, byrow=TRUE)

# The apparent conflict between the sheet, where a transpose was indicated, and
# the c command in R bothered many people, so here is the explanatation.

# In the sheet, the transpose is written to emphasize that b is a 'column'
# vector, i.e. an element of the vector space under discussion.

# Now, internally, 'in its own head' so to speak, R has two distinct variable
# types, one called vector and one called matrix. These behave differently to
# one another. In particular, there is no notion of the transpose of a vector,
# only of a matrix. The function c, as used above, creates a vector variable. R
# automatically 'thinks of' this as a 'column vector'. Indeed, as you found, if
# you multiply b by a 3 x 3 matrix such as A, R does not complain.

# The confusing thing about this is that when R displays such a variable, it
# lists it all on one line, in a row. Ignore the way R displays vector
# variables: b is a column vector.

# Now, if you do take transpose of b, t(b), a strange thing happens. Because R
# does not have a built-in notion of the transpose of a vector, but only of a matrix,
# it first converts b to a 3 x 1 matrix, and then transposes it, creating a 1 x
# 3 matrix. If you now take a second transpose, t(t(b)), you will generate a 3 x
# 1 matrix. If you display these two variables, you will see that the first
# displays as a row, while the second displays as a column. Note that in both
# cases, they now have row and column numbers. If you try to (matrix) multiply
# t(b) by A, you will get an error, as the dimensions are not correct. If you
# multiply t(t(b)) by a matrix, everything will be fine, and you will get back
# another 3 x 1 matrix, exactly as you do when you  matrix multiply b by A. In
# fact, in order to carry out the latter operation, b is converted to a matrix
# first. Thus matrix x vector gives back a matrix.

# Note that when the 'vector' is a matrix variable, some operators perform
# differently than when it is a vector variable. For example,

d = t(t(b))

A*d

# gives an error. This kind of pointwise multiplication for matrices is not
# defined in R unless the matrices have the same dimensions. Similarly for A -
# d. These operations work when the variable is a vector such as b, however.

################################################################################
#b)

# Add one to all elements of A.
A+1

# Substract b, a column, from all columns of A.
A-b

# Multiply all columns of A pointwise by b.
A*b

# Actual matrix multiplication.
A%*%b

################################################################################
# c)

# Determinant.
det(A)

# Transpose.
t(A)

# Inverse!
solve(A)

################################################################################
# d)

# Extract the diagonal of the argument M as a vector, and then sum it.
tr<-function(M){
  Trace <- sum(diag(M))
  return(Trace)
}

tr(A)

# Note that this will not give an error if M is not square.

################################################################################
# 1.2
################################################################################

################################################################################
# a)

# Generate 100 samples each of x and y.
x<-runif(100,-1,1)
y<-runif(100,0,1)

# Note that runif is not 'run if' but 'r unif', random sampling from a uniform
# distribution. There are also functions rnorm, etc. for other distributions.

################################################################################
# b)

# Plot the values.
plot(x,y)

################################################################################
# c)

# Combine the x and y values and compute the variance matrix.
Z<-cbind(x,y)
var(Z)

# Note that you have to stack x and y next to one another to get the full
# variance matrix. This consists not only of the variances of x and y but the
# covariance between them:

#     x                         y
# x   var(x) = cov(x, x)        cov(x, y) = cov(y, x)
#
# y   cov(y, x) = cov(x, y)     var(y) = cov(y, y)

################################################################################
# d)

# Generate 10,000 samples each of x and y.
x<-runif(10000,-1,1)
y<-runif(10000,0,1)

# Plot them.
plot(x,y)

# Combine the x and y values and compute the variance matrix.
Z<-cbind(x,y)
var(Z)

# Evidently, the result here is closer to the true value. This is to be
# expected. As the number of samples tends to infinity, the variance of the
# estimate tends to zero, and its value to the true value.



################################################################################
# 1.3
################################################################################

################################################################################
# a)

# Read in the scallops data.
#scallops <- read.table("http://www.maths.dur.ac.uk/stats/courses/statsIII/data/scallops.dat", header=TRUE)
scallops <- read.table("T:/MATHS/DMA0JE/scallops.dat", header=TRUE)



################################################################################
# b)

# Compute the mean and display it.
m <- colMeans(scallops[,c("long","lat")])

m
# long       lat
# -72.73215  39.91798

# Compute the variance and display it.
Sigma<- var(scallops[,c("long","lat")])
Sigma
#           long       lat
# long 0.2636210 0.2531269
# lat  0.2531269 0.3699811

# Compute the eigenvalues and eigenvectors.
eVs = eigen(Sigma)

eVs$values
# [1] 0.57545402 0.05814811
# Note that all eigenvalues are > 0, meaning Sigma is positive definite.

################################################################################
# c)

# Plot them.
plot(scallops$long, scallops$lat)

# The mean can then be added to the scatterplot via
points(m[1],m[2], col=2, pch="+")



################################################################################
# d)

# Plot the histograms.
hist(scallops$tcatch)

hist(scallops$y)

# We see that the distribution of log-abundances is far less skewed, and appears
# to be more 'normal'. Normality of the response is a standard assumption of
# regression models (which we will consider in Section 3).


