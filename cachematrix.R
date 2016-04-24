#Coursera Data Science Specialization
#R Programming
#Programming Assignment 2: Lexical Scoping
#
#
#Author: Jose Pineda
#Date: April '16

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cacheSolve: This function will compute the inverse of the special "matrix" returned
#by makeCacheMatrix above. cacheSolve should retrieve the inverse from the cache if the inverse has already been calculated (and the
#matrix has not changed).

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}
