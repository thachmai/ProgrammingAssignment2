## This function takes a matrix and returns a list that provides caching capability to the list
## and its mean.
## The returned list has the following elements:
##   get() returns the value of the matrix
##   set(y) replaces the existing matrix with the new matrix
##          calling this method will also wipe out the cached inverse matrix if available
##   getinversed() returns the cached inversed matrix
##   setinversed(i) stores the inversed matrix in the cache
makeCacheMatrix <- function(x = matrix()) {
  # this variable stores the inversed matrix 
  inversed <- NULL
  
  set <- function (y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function () x
  setinversed <- function (i) inversed <<- i
  getinversed <- function () inversed
  
  list(set = set, get = get, setinversed = setinversed, getinversed = getinversed)
}


## This function takes the list returned by makeCacheMatrix and either:
##   computes and returns the inversed matrix if a cache is not available
##   returns the cached inversed if available
cacheSolve <- function(x, ...) {
  inversed <- x$getinversed()
  if (!is.null(inversed)) {
    # print("cache hit")
    return(inversed)
  }
  
  # print("no cache")
  inversed <- solve(x$get(), ...)
  x$setinversed(inversed)
  inversed
}
