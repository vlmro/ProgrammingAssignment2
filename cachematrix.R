## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix, cacheSolve
## makeCacheMatrix has the following: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
              x <<- y
              inv <<- NULL
      }
      get <- function() {x}    
      setinverse <- function(inverse) {inv <<- inverse}
      getinverse <- function() {inv} 
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## write a short comment describing this function
## This is used to get the cache data

cachesolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){      
              message("getting cached data.")
              return(inv)    
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}