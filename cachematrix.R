## The functions compute the inverse of a matix and caches it for later retrieval

## makeCacheMatrix creates a list of 4 funtions: setmatrix, getmatrix, setInvMatrix,
## getInvMatrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  setmatrix <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  getmatrix <- function() x
  setInvMatrix <- function(inv) invrs <<- inv
  getInvMatrix <- function() invrs
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## cacheSolve first checks if the given matrix has a cached inverse already stored.
## if it is not cached, then it computes the inverse and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInvMatrix()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$getmatrix()
  invrs <- solve(data, ...)
  x$setInvMatrix(invrs)
  invrs
}
