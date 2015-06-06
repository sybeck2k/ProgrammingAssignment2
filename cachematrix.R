  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## Extends a matrix to cache its inverse value
  makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  
  
  ## Calculates or retrives from cache the inverse of a matrix
  ## generated with makeCacheMatrix
  cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("Getting cached data...")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
  }
