## Caching the Inverse of a Matrix:
## Storing a matrix and caching its inverse.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse  = setInverse,
       getInverse = getInverse)
  ## This function creates a special "matrix" object that can cache its inverse
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x', created by makeCacheMatrix
}
