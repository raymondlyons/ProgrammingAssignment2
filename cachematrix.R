## Functions to receive an invertible matrix and invert it efficiently.
## If same matrix has been submitted before, find a cache version of that matrix
##     instead of calculating the inversion
##  By R. Lyons  July 25 2014

## Receive the initial matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##  initialize matrix inversion object (variable) to null
  ##  establish both inversion "solved" matrix  and matrix parameter as external to this function
  
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    
  ##  Create functions to get external matrix parameter variable, move solved matrix to
  ##      external variables
  ##  Return these functions as a list (accessible to cachesolve function below )
    
    get <- function() x
    setinv <- function(slv) m <<- slv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  }
  
  ##  Calculate inversion ("solve") of the incoming matrix
  ##  If matrix is a repeat, extract inversion from cache instead of solving

  cachesolve <- function(x, ...){
    ##  make inversion matrix parameter external
    m <- x$getinv()
    
    ## check for existence of this matrix solution; return if it exists
    if (!is.null(m)){
      message("getting cached data")
      return(m)
    }
    ##  Calculate solution as this is first submission of the matrix
    ##  return inverted matrix
    
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
