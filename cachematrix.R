## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function





## MAKE VECTOR
## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to

## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeVector <- function(x = numeric()) {

 

     m <- NULL

     set <- function(y) {
          x <<- y
          m <<- NULL
     }

     get <- function() x
     
     setmean <- function(mean) m <<- mean
     
     getmean <- function() m
     
     list(set = set, 
          get = get,
          setmean = setmean,
          getmean = getmean)
}






## CACHE MEAN
## The following function calculates the mean of the special "vector"
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it `get`s the mean from the
## cache and skips the computation. Otherwise, it calculates the mean of
## the data and sets the value of the mean in the cache via the `setmean`
## function.

cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}



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
