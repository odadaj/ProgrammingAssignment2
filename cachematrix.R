## Prgramming assignment 2 to test on caching.

makeCacheMatrix <- function(x = matrix()) {
    ## Function makeCacheMatrix takes a matrix as an input and returns a list of four (4) functions
    ##    1. Function set assigns values in the environment
    ##    2. Function getmat is used to get the matrix
    ##    3. Function setinv is used to set the inverse matrix
    ##    4. Function getinv is used to get the inverse matrix
    ##    <<- Is used to assign value outside of the environment the function is define 
  
  inv <- NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(matrixinv) inv <<- matrixinv
  getinv <- function() inv
  list(set = set, getmat = getmat, setinv = setinv, getinv = getinv)
  }

cacheSolve <- function(x, ...) {
  
  ## Solves for the inverse of the matrix provided as input into makeCacheMatrix function. 
  ## cacheSolve takes as input the function list that was the output of the makeCacheMatrix function.
  ## Checks to see if value exists and returns the inverse. Fetches from cache if exists and computes if non existent.
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
