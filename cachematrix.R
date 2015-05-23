## Pair of functions to cache and retrieve the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates an object with the following methods:
  # set:    Takes input y and assigns that value to x within makeCacheMatrix env; resets m to NULL
  # get:    Returns the value of x
  # setinv: Assigns function solve to variable m within makeCacheMatrix env
  # getinv: Returns result of solve function 'm'
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## cacheSolve takes a makeCacheMatrix object and returns or creates a cached inverse of that matrix
  # if m exists, the function will return the cached version of m without recalculating
  # if m does not exist, it will calculate and return the inverse of x
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
