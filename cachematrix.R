## There are two functions here : makeCacheMatrix and cacheSolve
## makeCacheMatrix takes a matrix as input and returns a list
## of four functions : set, get, setinv and getinv.
## The output of makeCacheMatrix can be passed to cacheSolve
## function and the inverse of the matrix that was passed to 
## makeCacheMatrix, is returned. The matrix that is passed should
## be invertible.

## makeCacheMatrix : This function takes a matrix as an
## input. It then creates 4 functions: set, get, setinv
## and getinv and then creates a list with those 4 functions as
## output. The set function can be used to direcly set the matrix
## as a global object so that it can be accessed by cacheSolve function.
## The get function returns the matrix that is passed as argument.
## The setinv function sets the inverse of the matrix as a global variable.
## getinv returns the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(invrs) inv <<- invrs
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve : This function takes the output of makeCacheMatrix
## function and returns the inverse of the matrix passed to makeCacheMatrix
## function. It first checks if the inverse already exists for that matrix
## by using the getinv function of makeCacheMatrix. If it exists then it
## returns that inverse along with a message.
## If the inverse doesn't exist, then it gets the matrix by using the get
## function of makeCacheMatrix. It then calculates the inverse by using
## solve function and then uses the setinv function of makeCacheMatrix 
## to set the inverse so that it can be cached. Finally it outputs the 
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
