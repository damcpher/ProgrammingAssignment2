## Put comments here that give an overall description of what your
## functions do

## You can set the matrix, return it with get, set the inverse, 
## and return it with getinverse

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This solves the inverse if it isn't already solved. Note, it must be a square matrix to take an inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##Note: This code is what's in the example, but changed to solve for inverse instead of mean.