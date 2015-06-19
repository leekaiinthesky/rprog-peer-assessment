## cachematrix.R
##
## Since inverting large matrices is computationally intensive, we use the idea of
## a "cachematrix" whose inverse will be cached the first time it is calculated,
## so that subsequent calls to invert the same cachematrix will be fast.
## - makeCacheMatrix constructs a cachematrix.
## - cacheSolve returns the inverse of a cachematrix.

## makeCacheMatrix constructs a cachematrix from an ordinary matrix. The
## implementation is a list of four functions:
## - get gets the cached matrix.
## - set sets a new cached matrix.
## - getInverse gets the cached inverse, or NULL if none has been set.
## - setInverse sets the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inv <<- NULL
  }
  get <- function() x
  setCachedInv <- function(newInv) inv <<- newInv
  getCachedInv <- function() inv
  list(set = set, get = get,
       setCachedInv = setCachedInv,
       getCachedInv = getCachedInv)
}

## cacheSolve returns the inverse of a cachematrix (as a regular matrix).
## It returns a cached inverse if one exists. If none exists, it computes the
## inverse and caches it.
cacheSolve <- function(x, ...) {
  inv <- x$getCachedInv()
  if (!is.null(inv)) {
    message("Returning cached inverse.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setCachedInv(inv)
  inv
}
