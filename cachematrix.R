## These two functions are used in order to cache the result
## of the operation of inverting a matrix. These two functions
## work together, one creates a special vector able to manipulate
## a matrix, the other uses it in order to calculate and set the inverse

## makeCacheMatrix - This function creates a special vector with methods
## for setting and getting the matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve - This function accepts a special vector containing
## a matrix and its inverse. If uses the methods provided by the
## makeCacheMatrix vetcor in order to get the matrix itself and to get
## and set its inverse. If the inverse is not available it calculates it
## and then stores it for further use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
