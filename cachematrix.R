## A pair of functions able to respectively cache the inverse of matrix 
## and trigger the calculation of the inverse if this has not yet been done

## The function "makeCacheMatrix" stores a list containing methods
## to store and retrieve a matrix, and to store and retrieve a local copy of
## the inverse (or a flag that shows no inverse has been calculated)


makeCacheMatrix <- function(x = matrix()) {
  local_inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(ext_inv) local_inv <<- ext_inv
  getinv <- function() local_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function will return the cached value for the inverse, if it's 
## already been calculated, or calculate it and store it 
## in the x list, if it hasn't

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
