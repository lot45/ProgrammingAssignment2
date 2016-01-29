## Write a pair of functions to manage caching of the inverse of a matrix.
## The first function creates a special representation of a matrix such that
## its inverse can be cached and recalled from cache rather than computing
## it from scratch everytime its needed.
##
## the function "makeCacheMatrix" creates a special matrix which stores an invertibel matrix
## and caches its inverse.

makeCacheMatrix<-function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

##
## the function "cacheInverse" computes the inverse of the special matrix
## created with the function createMatrix. It first checks to see if the matix
## inverse has already been calculated and retieves it from the cache.
## Else, it computes the inverse, and stores it in the cache via the setMatrix
## function

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
