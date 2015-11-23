## Put comments here that give an overall description of what your
## functions do

## input x = matrix and sets up list of commands to be referenced in "CachSolve"
makeCacheMatrix <- function(x = matrix(numeric())) {
  mi <<- NULL
  set <- function(y) {
    x <<- y                     # cached original matrix input
    mi <<- NULL            # clear cached inverse of matrix 
  }
  get <- function() x         # get input data
  setmi <- function(inv) mi <<- inv
  getmi <- function() mi
  list(set = set, get = get,
       setmi = setmi,
       getmi = getmi)
}
## cacheSolve function
## inputs:  x=makeCache function list, y=matrix being checked
cacheSolve <- function(x, y, ...) {   
  orig <- x$get()
  inv <- x$getmi()
  if(!is.null(inv)) {
    if(identical(y,orig)) {
      message("getting cached data")
      return(inv)
    }
  }
  mi <- solve(y)
  x$set(NULL)
  x$set(y)
  x$setmi(NULL)
  x$setmi(mi)
  return(mi)
}
