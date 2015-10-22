## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function contains a list of functions, it can reset initial value of the matrix. It cache the inverse value
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## initial value
  set <- function(y) {  ##set 
    x <<- y
    inverse <<- NULL  ## <<-save value in the object outside this function so that it can be retrieved
  }
  get <- function() x  ## show x
  setinverse <- function(inv) inverse <<- inv ##set inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} ##make a list


## Write a short comment describing this function
## his function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
    ## Return a matrix that is the inverse of 'x'
