rm(list = ls())


## vector /  matrix algebra in R

## element-wise product for matrices with the same dimension
A = matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
B = matrix(c(6, 5, 4, 3, 2, 1), 2, 3)
A
B
A*B


## element-wise product for matrices of different dimensions
## scale multiplication
A
2*A

## vector and matrix: same n.rows
A
c(1,2)*A
A*c(1,2)


## matrix multiplication with appropriate dimensions
C = t(B)
C
A%*%B
A%*%C

## solve linear equation Ax = b with invertible A
A = matrix(c(1, 2, 4, 3), 2, 2)
b = c(1, 0)
A
b
solve(A)
solve(A)%*%b # returns a matrix of dimension 2X1
solve(A, b) # returns a vector of length 2


## eigen-decomposition of a matrix
A = matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
A
AtA = tcrossprod(A)  ## t(A)%*%A
AtA
tAA = crossprod(A)   ## A%*%t(A)
tAA
round(eigen(AtA)$values, 5)
round(eigen(tAA)$values, 5)
## singular value decomposition (SVD)
svd(A)
svd(A)$d^2 # compare them with the eigenvalues above
svd(t(A))
svd(t(A))$d^2 # compare them with the eigenvalues above




## "apply()" without loop
X = cbind(rnorm(10), rnorm(10, 1), rnorm(10, 2, 1.5))
apply(X, 1, mean)
apply(X, 2, mean)
rowMeans(X)
colMeans(X)

npositive = function(v) sum(v>0)
apply(X, 1, npositive)
apply(X, 2, npositive)

## median of all columns
apply(X, 2, median)

## Interquartile ranges of all columns 
apply(X, 2, IQR)

## coefficients of variation of all columns
coefvar = function(v) sd(v)/mean(v)
apply(X, 2, coefvar)


## boxplot of all columns of a matrix
colnamesofX = c("N(0,1)", "N(1,1)", expression(N(2, 1.5^2)))
boxplot(X, names = colnamesofX)

