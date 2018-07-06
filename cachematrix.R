## Put comments here that give an overall description of what your
## functions do

## this captures the make cache matrix, which mirrors the cache mean with inverse operations in place of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinverse = function(inverse) i <<- inverse 
  getinverse = function() i
  list(set=set, get=get, setinv=setinverse, getinv=getinverse)
}


## this gets the cache values if there is a matrix already in the environment

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  i = x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data = x$get()
  i = solve(data, ...)
  x$setinverse(i)
  i
}
