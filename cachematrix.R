makeCacheMatrix <- function(x = matrix()){
      mir <- NULL
      set <- function(y){
              x <<- y
              mir <<- NULL
      }
      get <- function() x    
      setinverse <- function(inverse) mir <<- inverse
      getinverse <- function() mir
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This is used to get the cache data

cachesolve <- function(x, ...) {
      mir <- x$getinverse()
      if(!is.null(mir)){      
              message("getting cached data-")
              return(mir)    
      }
      val <- x$get()
      inv <- solve(val, ...)
      x$setinverse(val)
      val
}