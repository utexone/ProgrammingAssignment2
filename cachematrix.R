## Caches the inverse of a matrix 
# and looks for the cache before recalculating the inverse

## Chaches the inverse i of a matrix x
## Use x <- cacheMatrix (x) to be able to call cacheSolve(x) later!

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y) {
               x <<- y
               i <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) i <<- solve
          getinverse <- function() i
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Gets the cache of the inverse of matrix x
## If no cache is available, it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
          if(!is.null(i)) {
               message("getting cached data")
               return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
}
