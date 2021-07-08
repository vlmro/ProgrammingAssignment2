## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix, cacheSolve
## makeCacheMatrix consists of set, get, setinverse, getinverse
## library(MASS) is used to calculate inverse for non-squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
              x <<- y
              inv <<- NULL
      }
      get <- function() {x}     ## function to get matrix x
      setinverse <- function(inverse) {inv <<- inverse}
      getinverse <- function() {inv} ## function to obtain inverse of the matrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## write a short comment describing this funtion
## This is used to get the cache data
cachesolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){      ## checking wheter inverse is Null
              message("getting cached data!")
              return(inv)     ## returns inverse value
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}