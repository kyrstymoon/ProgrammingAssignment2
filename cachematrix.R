## Functions to create a matrix (with getter and setter functions)
## and solve the inverse of the created matrix if it doesn't already
## exist in memory.


#############################################################################
## Function: makeCacheMatrix(x = matrix())
## Purpose: create a matrix with getter and setter functions and the 
## functionality to compute the inverse.
#############################################################################
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y ##accessible outside
    m <<- NULL ##accessible outside
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve #solve and store in m
  getinverse <- function() m #get the inverse already set
  list(set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}


#############################################################################
## Function: cacheSolve
## Purpose: Store the inverse of the matrix in memory if it doesn't already
## exist, otherwise retrieve it.
#############################################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() #try to get the object in memory
    if(!is.null(m)){ #retrieve cached data
      message("getting cached data")
      return(m)
    }
    data <- x$get() #matrix to invert
    m <- solve(data, ...) 
    x$setinverse(m) #set the inverse on the object
    m
}
    
