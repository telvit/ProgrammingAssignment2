## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## this function takes an argument of a numeric matrix
## and returns a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## this function computes the inverse of a matrix.
## if the inverse has already been computed the
## function returns its cached data, or else
## it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinverse(invmatrix)
  return(invmatrix)
  }
