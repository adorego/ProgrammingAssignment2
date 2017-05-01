##These functions are for optimizing computations calculations storing them in the parent environment of the functions

## MakeCacheMatrix creates an object with 6 methods inside which are:
## set: To set the value of the matrix passed by argument, this value is stored in the parent environment
## get: To get the value of the current matrix
## setInverse: To calculate the inverse of the matrix, it is stored in the parent environment
## getInverse: Gte the value of the calculated and stored inverse matrix
## getChanged: Get TRUE if the value is changed and its inverse never calculated
## setChanged: To set the Chnaged value to FALSE after an inverse calculation

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x<<- y
    invMatrix <- NULL
    changed <<- TRUE
  }
  get <- function() x
  setInverse <- function(inv) {
    invMatrix <<- inv
    changed <<- FALSE
  }
  getInverse <- function() invMatrix
  getChanged <- function() changed
  setChanged <- function(change) changed <- change
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse,
       getChanged = getChanged,
       setChanged = setChanged)
}


## This function return the inverse of the matrix passed by argument, if this calcualtion was
## already stored, then it pass the stored value otherwise it makes the calculation and stores
## the value

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv) & x$getChanged()==FALSE){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
