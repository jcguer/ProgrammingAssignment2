## The functions return the inverse of a matrix either 
## from a cache or by calculating the inverse and then 
## cacheing it.

## makeCacheMatrix creates a list of functions 
## ($get, $setinverse, $getinverse) that
## that cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      
      list(get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
      
}



## cacheSolve uses !is.null(m) to check whether the inverse of a 
## Matrix m has been cached- if it has, it returns the message
## "getting cached data" and the cached inverse. If not, it 
## finds the inverse using solve() and then caches it using 
## x$setinverse(m).


cacheSolve <- function(x, ...) {
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
      
}


