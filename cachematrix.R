## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly. Below are a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) invMatrix <<- inv
  getInverse <- function() invMatrix
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached matrix")
    return(invMatrix)
  }
  m <- x$get()
  ## Return a matrix that is the inverse of 'm'
  invMatrix <- solve(m, ...)
  x$setInverse(invMatrix)
  invMatrix
}
