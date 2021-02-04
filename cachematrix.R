## This is the solution to Programming Assignment 2 of R Programming course.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

# set the value of the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
 # get the value of the matrix
  get <- function() x

 # set the value of the inverse matrix
  setInverse <- function(solveMatrix) inverse <<- solveMatrix

# get the value of the inverse matrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## "cacheSolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  non_inverse <- x$get()
  inverse <- solve(non_inverse)
  x$setInverse(inverse)
  inverse      
}
