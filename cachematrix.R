## cachematrix.R
## Coursera: R Programming
## Student: Emilie H. Wolf

## This script both solves a matrix (by finding its inverse)
## and stores the inverse matrix in memory so it can be retrieved
## without recomputing


## This function creates get/set functions to assign values to
## the matrices in the parent frame so computations aren't lost
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                             
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## This function finds the inverse matrix, either by solving it
## computationally, or by retrieving it from memory
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
