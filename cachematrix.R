## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a matrix that can cache the inverse of the matrix
## cacheSolve computes the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initialising inverse as NULL
  set <- function(y) {
    x <<- y # "<<-" encloses the environment of the parent function, and can access all variables and parameters in that function. It allows us to have two levels of parameters. The parent controls how the function works and the child does the work.
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <-function(inverse) {inv <<- inverse}
  getinverse <<- function() {inv} ##get inverse of matrix
  #creates list for get/set 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




cacheSolve <- function(x, ...) {  ##gets cache data
  inv <- x$getinverse()
  if(!is.null(inv)) { ## checking that inverse is not NULL
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
