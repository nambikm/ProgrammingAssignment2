## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function encapusates the matrix in a list.
## The function returns a list which contains function through which the encapusated matrix and it's inverse could be accessed.


makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  
  getMatrix <- function() x
  setMatrix <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  getInvMatrix <- function() invMat
  
  setInvMatrix <- function( matInv ) invMat <<- matInv
  
  
  list( getMatrix = getMatrix, setMatrix = setMatrix, getInvMatrix = getInvMatrix, setInvMatrix = setInvMatrix )
}

## Write a short comment describing this function
## This function checks to see if the inverse of the matrix is already been calcuated.
## if not calculates the inverse and caches it and returns the inverse. If it is already cached it just returns the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInvMatrix()
  if( !is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  
  mat <- x$getMatrix()
  inverseMatrix <- solve(mat)
  
  x$setInvMatrix(inverseMatrix)
  
  inverseMatrix
}