
## Create a cache of a matrix. It's really a list of
## four functions. The first sets, the second gets
## the third caculates, and the fourth gets again.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     get <- function() x
     
     setmatrix <- function(matrix) m <<- matrix
     
     getmatrix <- function() m
     
     list(set = set, 
          get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## Take a matrix as input. Assume its an invertible square.
## If it's already solved and cached, return that.
## If not, do the work and solve it and return that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getmatrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setmatrix(m)
     m
}
