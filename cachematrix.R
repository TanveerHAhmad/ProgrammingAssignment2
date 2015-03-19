## cachematrix.R - R Programming Assignment 2 - Week 3 
## Programming Assignment 2 is to write a pair of functions that cache the inverse
## of a matrix.
## The following functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can
##    cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been calculated 
##    (and the matrix has not changed), then the cachesolve should retrieve the 
##    inverse from the cache.

## makecachematrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cachesolve function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run 1:
##> source("makecache_cachesolve_matrix.R")
##> m <- matrix(c(-1, -2, 1, 1), 2,2)
##> x <- makeCacheMatrix(m)
##> x$get()
##[,1] [,2]
##[1,]   -1    1
##[2,]   -2    1
##> inv <- cacheSolve(x)
##> inv
##[,1] [,2]
##[1,]    1   -1
##[2,]    2   -1
##> inv <- cacheSolve(x)
##getting cached data.
##> inv
##[,1] [,2]