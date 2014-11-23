##As requested in the Programming assigment 2, bellow you can find the functions 
##that cache the inverse of a matrix.


## the function of a special matrix object to cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Starts inversion property
  i <- NULL
  
  ## Sets the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## getting the matrix
  get <- function() {
    
    ## matrix returning
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    
    ## inverse property return
    i
  }
  
  ## Returning a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The follwowing function, as requested, Computes the inverse of a special matrix
##returned by the function "makeCacheMatrix" (as above).
##In case the inverse has been previously calculated (and the matrix has not
## changed), then the "cachesolve" retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Returns a inverse of "x" matrix
  m <- x$getInverse()
  
  ## Condition to return the inverse if it had been set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Getting the matrix from the object
  data <- x$get()
  
  ## Calculation of the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Setting the inverse to the object
  x$setInverse(m)
  
  ## matrix returning
  m
}