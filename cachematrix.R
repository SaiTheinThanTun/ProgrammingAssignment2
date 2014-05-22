##The following 2 functions caches the inverse of a matrix

## "makeCacheMatrix()" function stores a matrix and caches it's values. 
## It also caches its inverse (produced from the second function).
## Output is a list of functions which caches the values of matrices (both input & its inverse)
## Usage: Assign "makeCacheMatrix()" function to a symbol
## EG., x <- makeCacheMatrix()
## This way, 1 of the functions within the function can be used to set the matrix values.
## EG., x$set(matrix(1:4,2,2)) 
## It will use the "set" function within the "makeCacheMatrix()" function
## to store a 2 by 2 matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve()" function checks if there's any cached value and
## returns the cache. Otherwise, it re-caculates the inverse, 
## stores/ caches the new inversed value in the previous function,
## and returns the newly inversed matrix.
## Usage: cacheSolve(x) ## where "x" is the object into which the first function is assigned.
## To check:> matrix(1:4,2,2) %*% cacheSolve(x) ## should return an identity matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("retrieving cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
  ## Return a matrix that is the inverse of 'x'
}
