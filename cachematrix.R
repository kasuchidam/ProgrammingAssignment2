# The first function, makeCacheMatrix creates a list containing  functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setinverseMatrix <- function(inverseMatrix) inv <<- inverseMatrix
  getinverseMatrix <- function() inv
  list(setMatrix=setMatrix, getMatrix=getMatrix, setinverseMatrix=setinverseMatrix, getinverseMatrix=getinverseMatrix)
}


# cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# inverse computation. If not, it computes the inverse, sets the value in the cache through 
# setinverseMatrix function.

# Assumption : the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$setinverseMatrix(inv)
  inv
}