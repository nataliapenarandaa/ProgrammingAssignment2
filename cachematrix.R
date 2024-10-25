## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse to NULL

  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get the cached inverse
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  # Get the matrix
  data <- x$get()
  
  # Calculate the inverse
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  inv  # Return the inverse
}

