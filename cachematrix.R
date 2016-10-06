#                       This code contains two functions: 
#                 makeCache Matrix(x = matrix()) and cacheSolve(x,...)
#
#                           makeCacheMatrix():
#
# makeCacheMatrix() is essentially a list of functions that performs the following:
#
# it creates a special matrix, stores the matrix, retrievs it and passes 
# to cacheSolve() to calculate an inverse. After the matrix inverse is calculated
# makeCacheMatrix() saves in its object environment and retrieves it when needed
# with no need for recalculation. 
#
#                           cacheSolve():
#
# cacheSolve() calculates an inverse matrix if a new matrix is passed to makeCacheMatrix() 
# and 'inv' variable is nulled. cacheSolve() calculates the inverse matrix and passes it back 
# to makeCacheMatrix, so the inverse is saved in makeCacheMatrix() object's environment.
#
# If no new matrix is passed to makeCacheMatrix, the inverse matrix calculation 
# is avoided and it is retrevied from the cache.
#
#    For more details, please read the comments in the functions below




makeCacheMatrix<-function(x = matrix()){
  
  
  # x argument of matrix class is initialized at the declaration of the function above
  #
  # inv <- NULL initializes inv variable to store an inverse matrix
  # in makeCacheMatrix() object's environment:
  inv <- NULL 
  
  # set function receives a new matrix ( x<<- y), stores it in its parent's environment and 
  # clears the previous inverse value stored in parents environment as well. 
  # It is accomplished through setting inv object to NULL ( inv <<- NULL ):
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function fethces the matrix x stored in the parents function's object's environment: 
  get <- function() x
  
  # setinv calls up solve for inverse matrix calculation and stores it as inv
  # in the function's object's environment:
  setinv <- function(solve) inv <<- solve
  
  # getinv retrieves inverse inv from the object's environment:   
  getinv <- function() inv
  
  # list of named functions to be returned to the parent and to be used with $ extract operator    
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}
cacheSolve <- function(x, ...) {
  
  # retrieval of the 'inv' inverse matrix from cache:
  inv <- x$getinv()
  
  # if the retrieved object is not a null i.e. if(!is.null(inv)) = True
  # then "getting cached data" message is printed to the console,
  # the cached inverse matrix is returned and the function is exited:
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # data <- x$get() retrieves a new matrix: 
  data <- x$get()
  
  # inv <- solve(data, ...) calculates an inverse matrix: 
  inv <- solve(data, ...)
  
  # x$setinv(inv) stores the inverse matrix in makeCacheMatrix() object's environment: 
  x$setinv(inv)
  
  # returns the inverse matrix: 
  inv
}

#The following R Code was used to test the functions:

#mat<-matrix(c(1,2,3,4,5,3,2,4,4),3,3)
#> m1<-makeCacheMatrix(mat)
#> cacheSolve(m1)


# Original matrix:
#
#       [,1] [,2] [,3]
# [1,]    1    4    2
# [2,]    2    5    4
# [3,]    3    3    4
#
# The inverse  matric calculated:
#       [,1]       [,2]      [,3]
#[1,]  1.3333333 -1.6666667  1.0
#[2,]  0.6666667 -0.3333333  0.0
#[3,] -1.5000000  1.5000000 -0.5
#
#
# The Inverse matrix is tested by matrix multiplication of the inverse and the original matrices:
# m2 %*% mat
# The product result is an indentity matrix:
#        [,1]     [,2]         [,3]
#  [1,]    1 0.000000e+00 8.881784e-16
#  [2,]    0 1.000000e+00 0.000000e+00
#  [3,]    0 2.220446e-16 1.000000e+00
#
#