## <<- operator is used to assign a value to an object in an environment that 
## is different from the current environment.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inv <- inverse}
  getInverse <- function() {inv}
  list(set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## the following function calculates the mean of the special "vector" 
## created with the above function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) ## Return a matrix that is the inverse of 'x'
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
