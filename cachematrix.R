## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##  get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse 
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated, then the "cachesolve" 
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  ## Just return the inverse if already set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse
  i <- solve(data, ...)
  
  ## Set the inverse to the object
  x$setInverse(i)
  
  ## Return the matrix
  i
}

