## Put comments here that give an overall description of what your
## functions do

# makeChacheMatrix creates a matrix object and defines the getters and setters for it
# it. It also has getter and setter for the inverse.
# cacheSolve returns an inverse of the matrix. If it is in the chache it will just
# return the chaged object. If it is NULL (not in the chache) it will compute it.


## Write a short comment describing this function
# this function computes a matrix object and sets/caches the inverse. The matrix object
# is stored in the global environment. It does NOT compute the inverse.
# But it sets and gets the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## the inverse is initialized with NULL
  set <- function(y) { ## set function for the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## get function for the matrix
  setInverse <- function(inverse) inv <<-- inverse ## set function for the inverse matrix
  getInverse <- function() inv ## get function for the inverse matrix
  # put function in a list so that an object is returned and functions can be called from environment
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function
# This function checks if the inverse exists (if it is not NULL).
# It it exists (and thus the matrix was not changed) the inverse is returned.
# Otherwise the inverse is computed and chached.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # get the inverse matrix
  if(!is.null(inv)) { # if it is not NULL return it
    message("getting cached data")
    return(inv)
  }
  matr <- x$get() # else get the matrix and ...
  inv <- solve(matr, ...) # solve for the matrix inverse and...
  x$setInverse(inv) # set the matrix inverse (with the setter of makeCacheMatrix)  and...
  inv # return the matrix inverse 
}
