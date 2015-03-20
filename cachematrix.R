## cachematrix.R
## Coursera: R Programming
## Student: Emilie H. Wolf

## This script both solves a matrix (by finding its inverse)
## and stores the inverse matrix in memory so it can be retrieved
## without recomputing


## This function creates get/set functions to assign values to
## the matrices in the parent frame so computations aren't lost
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x                             
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## This function finds the inverse matrix, either by solving it
## computationally, or by retrieving it from memory
cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
  ## Return a matrix that is the inverse of 'x'
}
