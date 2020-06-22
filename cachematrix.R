## the function will cache the inverse of a matrix ,time efficiently
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## Initialize the inverse property
      inverse <- NULL
## Method to set the matrix
      set <- function(y) {
    x <<- y
    m <<- NULL
  }
## Method the get the matrix
      get <- function() {
   
         ## Return the matrix
    x
  }
  ## Method to set the inverse of the matrix
  setinverse <- function(i) {
    inverse <- i
  }
  ## Method to get the inverse of the matrix
  getinverse <- function(){
    ## Return the inverse property
    inverse
  }
  ## Return the inverse property
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
## It will compute the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  ## Just return the inverse if its already set
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  inverse <- solve(data, ...)
  ## Set the inverse to the object
  x$setinverse(inverse)
  ## Return the matrix
  inverse
}