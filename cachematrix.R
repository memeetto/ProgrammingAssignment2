## Caching the Inverse of a Matrix:
## Matrix inversion is an approach to cach the inverse of a matrix rather than compute it 
## each time the code is executed.
## The makeCacheMatrix function creates a special object that stores a matrix.
## The cacheSolve function generates and caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the makeCacheMatrix 
## above. Provided the matrix inverse has already been calculated (and the 
## matrix has not changed) the function will then retrieve the inverse from the cached matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
