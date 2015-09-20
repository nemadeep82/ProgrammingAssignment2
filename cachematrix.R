## File Name : cachematrix.R
## Version : 1.0
## Date :
## This R script provides functions to create a special matrix object which stores
## the cached value of its inverse. If caches value doesn't exist then it computes 
#  it and cache it for future user.

## This function creates a special type of matrix object which can cache it's 
## inverse value.This matrix object also provides multiple functions which can be 
## called to set/get the matrix value and set/get its inverse.
##
## Input Parameters
##
## x - Matrix
##
## Output
##
## List of functions-
## set - To set the matrix value
## get - To get the matrix value
## setinverse - To set the inverse value of the matrix
## getinverse - To get the cached inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inv)
    mtx <<- inv
  getinverse <- function()
    mtx
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function provide functionality to compute and get the inverse of the
## special matrix. It first looks into the cache. If the inverse is already 
## storedin the cache then returns it or else compute it and cache it for 
## future use.
##
## Input Parameters
##
## x - Special Matric Object created by function makeCacheMatrix
## .. - Other parameters required for inverse matrix (Solve) function.
##
## Output
##
## Inverse value of the matrix


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtx <- x$getinverse()
  if (!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data, ...)
  x$setinverse(mtx)
  mtx
}