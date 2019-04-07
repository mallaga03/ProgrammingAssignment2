## This package contains two functions: makeCacheMatrix and cacheSolve.
## The first function makeCacheMatrix returns a special matrix that holds the inverse.
## cacheSolve is a function that calculate and cache the inverse of a matrix.

## makeCacheMatrix is a function that create a special matrix that holds the inverse as well.
## the "special matrix" contains 4 functions:
## 1. get the matrix.
## 2. set the matrix, this function will initialize the matrix and set the inverse to NULL
## 3. getSolve to return the inverse of the matrix
## 4. setSolve to set the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {

  # Initialize the inverse
  inverse <- NULL
  
  # set function initialize the matrix and the inverse
  set <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  
  # get function to get the matrix
  get <- function () x
  
  # set inverse value
  setSolve <- function(i) {
    inverse <<- i
  }
  
  # get the inverse
  getSolve <- function() inverse
  
  # return the list of functions
  list(set = set,
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve is a function that returns the value of the inverse of a matrix x
## if the inverse is calculated, it will be returned from the cache
## otherwise it will be calculated, cached and then returned.
cacheSolve <- function(x, ...) {
  # get the cached inverse
  s <- x$getSolve()
  # if the inverse is already calculated, return it.
  if (!is.null(s)) {
    print('Retrieved the inverse from cache')
    return(s)
  }
  # otherwise calculate it
  print('Calculating inverse matrix and caching it')
  # first get the matrix
  m <- x$get()
  # then calculate the inverse
  s <- solve(m, ...)
  # then cache the inverse
  x$setSolve(m)
  # finally return the calculate inverse
  s
}
