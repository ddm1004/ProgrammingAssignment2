## Functions to cache a matrix inverse to speed up use of the matrix

## Function that takes a matrix to invert and returns a list of function 
## pointers to get or set the matrix and the inverse of the matrix.  
## The matrix must be invertible.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function to return the inverse of a matrix.  It takes a list of function 
## pointers built around a matrix.  If it is in the cache, it will return 
## the inverse from there, otherwise it will calculate the inverse,
## and store it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if (!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinverse(x_inv)
  x_inv
}
