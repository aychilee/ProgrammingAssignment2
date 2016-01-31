## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## Provides a list of functions that will either:
## set <- set matrix
## get <- get matrix that has been stored
## setMatInv <- set matrix inverse
## getMatInv <- get matrix inverse that has been stored

makeCacheMatrix <- function(x = matrix()) {
  
  mat <- x
  matInv <- NULL
  
  # create a function that sets the stored matrix
  setMatrix <- function(y) {
    mat <<- y
    matInv <<- NULL
  }
  
  # create a function that gets the stored matrix
  getMatrix <- function() mat
  
  # create a function that stores the inverse of the matrix
  cacheInverse <- function(matrix) matInv <<- matrix
  
  # create a function that gets the stored inverse of the matrix
  getInverse <- function() matInv
  
  # return the different functions available in this object so that they can be called
  list(
      setMatrix = setMatrix
      ,getMatrix = getMatrix
      ,cacheInverse = cacheInverse
      ,getInverse = getInverse
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get the stored matrix inverse
  matInv <- x$getInverse() 
  
  # check to see if the matInv has value, if so then return the matInv
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  
  # since there is no matInv, create one
  data <- x$getMatrix()
  matInv <- solve(data, ...)
  
  # store in x using the special function
  x$cacheInverse(matInv)
  
  # return the matInv
  matInv
}
