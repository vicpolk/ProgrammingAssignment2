## Assignment: Caching the Inverse of a Matrix
## These functions return the inverse of a matrix, store it in cache and retrieve from cache 
## this saves computation power if the inverse of the same matrix needs to be calculated again

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse )
}


## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve  
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

