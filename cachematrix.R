## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Set x to be a square matrix and allow it to return a list containing functions to
## make the matrix, get the matrix, set the inverse, and then return the inverse.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}

## For this function, make x the output of makeCacheMatrix()
## Then return the inverse of the matrix input to makeCacheMatrix()
## If the inverse has already been calculated, get it from the cache and 
## skip the computation. 
## Else, calculates the inverse and set the value of the inverse 
## in the cache via the setinv function. Then return the inverse of the matrix.