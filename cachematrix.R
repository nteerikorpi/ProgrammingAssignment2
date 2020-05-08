## COURSERA COURSE: R Programming

## ASSIGNMENT 2 - Caching the Inverse of a Matrix: 
## Below are a pair of functions that are used to create a special object that
## stores a matrix and chaches its inverse. 
## functions do

## FUNCTION 1: 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setInverse(inv)
  inv
}



