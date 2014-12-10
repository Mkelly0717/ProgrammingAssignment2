#####################################################
## Author: Michael A. Kelly Ph.D.
## Class: Coursera R Programming.
## Homework Assignment 2.
## Create Date: December 9, 2014.
## Changes:                     
#####################################################
## This is a function takes a matrix as an argument. 
##   The argument should be like matrix(c(1,2,3,4),2,2).
##   example: A <- makeCacheMatrix(matrix(c(1,2,3,4),2,2)
##   where A will be the argument to cacheSolve below.
##   The Matrix A is then created in the parent environment.    
##    Four function are returned in the list. 
##    1) get: will return the current matrix that was passed
##        as an argument. -> A$get()
##    2) set : will set assign a new matrix to be inverted.
##    3) setinv *** DO NOT CALL Directly -will corrupt problem**
##    4) getinv: will return the cached inverted matrix.
##       i.e, you need to run cacheSolve before getinv() will
##       return a non NULL value.

makeCacheMatrix <- function(x = matrix()) {
  
  
  # Initialine the inv matrix in the current scope
  inv <- NULL

  # create a function to set the matrix in the parent env.
  # and null the inverted matrix for a new solve.
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  # create a function to get the entered matrix.
  get <- function() x

  # create a function to set solve and set the inv matrix
  setinv <- function(solve) inv <<- solve

  # create a function to get the inverted matrix. (must be cached first)
  getinv <- function() inv

  # return the list
  list (set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
}  

## This function is to be used in conjunction with makeCacheMatrix.
##   cacheSolve will check to see if the inveted matrix exists in the cache.
##   if it does not then it will calculate the inverse and cache it.
##   otherwise it will use the cached inverted matrix.
cacheSolve <- function(x, ...) {

  # get the inverted matrix
  inv <- x$getinv()

  # If a value was returned. Print message using the cache and return.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # if inv was null then get the matrix and assign to data.
  data <- x$get()

  # calcuate the inverse and assign to inv.
  inv <- solve(data)

  # set the newly calculate inverse.
  x$setinv(inv)

  # return the inverted Matrix
  inv
}

