## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
      invr <- NULL
      set <- function(y) {
            x <<- y
            invr <<- NULL
      }
      get <- function() x
      setinvr <- function(inverse) invr <<- inverse
      getinvr <- function() invr
      list(set = set, get = get, setinvr = setinvr, getinvr = getinvr)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      cacheSolve <- function(x, ...) {
      invr <- x$getinvr()
      if(!is.null(invr)) {
            message("getting cached result")
            return(invr)
      }
      data <- x$get()
      invr <- solve(data, ...)
      x$setinv(invr)
      invr
}

