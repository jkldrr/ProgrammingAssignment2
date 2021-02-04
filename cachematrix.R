## This is the solution to Programming Assignment 2 of `R Programming` class

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # variable to hold the inverse
  
  # set the value of the matrix
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  
  # get the value of the matrix
  get <- function()
    x
  
  # set the value of the inverse matrix
  setInverse <- function(inv)
    inverse <<- inv
  
  # get the value of the inverse matrix
  getInverse <- function()
    inverse
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
  
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  # inverse matrix is cached
  if (!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  
  # inverse matrix is not cached -> calculate and cache it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
