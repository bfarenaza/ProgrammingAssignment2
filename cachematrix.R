## Matrix inversion is a costly computation. This file contains a couple
## of functions for caching the inverse of a matrix rather than compute it 
## over and over again.


## ---------------------------
##
## This function creates a special "matrix" object that can cache its inverse
## The matrix is really a list containing functions to set and get the
## value of the matrix and to set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # sets the value of the matrix
  # when matrix changes inverse is set to NULL so to not cache the old value
  set <- function(y) {
    x <<- y
    inverse <<- NULL         
  }
  
  # gets the value of the matrix
  get <- function() x
  
  # sets the value of the inverse
  setinverse <- function(inv) inverse <<- inv
  
  # gets the value of the inverse
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## ---------------------------
##
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # 1. Get the inverse matrix from the object itself
  inv <- x$getinverse()
  
  # 2. If the inverse is not null, then it has already been computed
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #3. If inv is null, then we need to calculate the inverse and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
