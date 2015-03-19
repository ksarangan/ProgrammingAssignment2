## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix.  Includes functions to set matrix values, get the matrix,
## set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  get <- function() x
  setinverse <- function(amatrix) invmat <<- amatrix
  getinverse <- function() invmat
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of a matrix after checking that there
## isn't an inverse on the cache.  If not, it computes it and sets the
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    imat <- x$getinverse()
    if(!is.null(imat)) {
        message("getting cached data")
        return(imat)
    }
    
    mat <- x$get()
    imat <- solve(mat)
    x$setinverse(imat)
    imat
}
