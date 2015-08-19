## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set - function that defines the given matrix 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get - function that returns the given matrix  
  get <- function() x
  #setsolve - function that defines the inverse of the given matrix
  setsolve <- function(solve) m <<- solve
  #getsolve - function that returns the inverse of the given matrix
  getsolve <- function() m
  #list that returns the gets and sets for the given matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve is a function that computes the inverse of the given matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  #m is the inverse of the given matrix
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
