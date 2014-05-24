## makeCacheMatrix and cacheSolve functions are used to 
## cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  
  ## if an object is called without a method
  
  ## create a NULL object
  m <- NULL
  
  ## sets the value of Matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of Matrix 
  get <- function() x
  
  ## set the inverse value of Matrix
  setinverse <- function(inverse) m <<- inverse
  
  ## get the inverse value of Matrix
  getinverse <- function() m
  
  ## Returns a list of functions to access the variables in makeCache function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If inverse has already been calculated then this function will retrieve the inverse from
## the cache
cacheSolve <- function(x, ...) {
    ## get the matrix inverse from makeCacheMatrix function 
    m <- x$getinverse()
    
    ## if is not null print "getting cached data" 
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    
    ## get matrix from makeCacheMatrix function
    data <- x$get()
    
    ## calculate the inverse
    m <- solve(data, ...)
    
    ## set the matrix inverse
    x$setinverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
