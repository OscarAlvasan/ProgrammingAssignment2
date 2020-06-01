## These functions cache the value of the inverse of a matrix

## This function creates a matrix which contains a function to set the value of
## a matrix, get the value of the matrix, set the value of the inverse and get
## the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This calculres the inverse of the matrix checked first if the inverse has
## already been calculated if so, it gets the inverse from the cache and skips
## the computation

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
