## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL 
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {
    inver<-ginv(x)             #function to obtain inverse of the matrix
    inver%*%x
    }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This is used to get the cached data

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
