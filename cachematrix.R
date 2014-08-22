## The first function creates a matrix with getters and setters for storing the inverse
## of matrix and get it when required. Second function first checks the cache if the inserse 
## for the required matrix is already available in the cached variable 'm', If so it doesn't
## calculate the inverse rather fetches the one in cache. If not then inverse of matrix is 
## caculated and stored in the the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  
  m  
  
}
