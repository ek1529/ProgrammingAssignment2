## The first function, makeCacheMatrix, generates a list of four functions. This list
## is used in the argument of the second function, cacheSolve. CacheSolve gives you
## the inverse of the matrix passed as an argument to the first function makeCacheMatrix.
## CacheSolve saves memory because it only calculates the inverse if the value of x 
## (the input matrix) changes. It is important that the output of makeCacheMatrix is
## saved in a new variable, which then is passed to the cacheSolve function.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

## x is initialized as an empty matrix and inv as NULL
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
## updating the x and inv values found in parent environment
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    return (x) 
  }
## The inverse of a matrix is calculated with the Solve function  
  setinverse <- function(solve) {
    inv <<- solve
  }
  getinverse <- function() {
    return (inv)
  }
## Output is defined as a list, which subsequently can be saved in a variable
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
## Only if the inverse has already been calculated, this function is executed
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## If the inverse was not yet calculated, it is calculated here
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
