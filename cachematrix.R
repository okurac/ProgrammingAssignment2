## Part 1- makeCacheMatrix:
## Caching the inverse of a matrix saves time and condenses code.

## The function below creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
  }
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set = set, 
         get= get,
         set_inv = set_inv,
         get_inv = get_inv)
}

#Part 2- cacheSolve
#Computing the inverse of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return inverse of matrix 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
       print("getting cached data")
       return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  inv
}
