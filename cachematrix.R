## the following 2 functions help in caching matrix and its inverse information
## it computes the new values if the inverse doesnt exist and caches it.

## makeCacheMatrix() takes an invertible matrix as its input
## it returns a list whose elements are 4 functions
## let's say x is an invertible matrix
## if we assign a <- makeCacheMatrix(x)
## a$set(newmatrix) will cache the newmatrix
## a$get() will get the matrix that has been cached
## a$setinverse(inverseofx) will cache inverseofx as the inverse of x matrix
## a$getinverse() will get the inverse matrix that has been cached

makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  set <- function(y){
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inversemat <<- inv
  getinverse <- function() inversemat
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
  
}


## continuing from the previous function
## cacheSolve() takes a list of functions as its input and
## cacheSolve(a) returns the inverse of a matrix
## a from the above comments for makeCacheMatrix()
## It computes the inverse if inverse is not cached and caches the new value and returns the new inverse
## if it is cached it just uses the cached inverse matrix and returns it


cacheSolve <- function(x, ...) {
  inversemat <- x$getinverse()
  if(!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)
  }
  data <- x$get()
  inversemat <- solve(data, ...)
  x$setinverse(inversemat)
  inversemat
}