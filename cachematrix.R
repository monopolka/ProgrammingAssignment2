## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # returns the matrix
  get <- function() x
  
  # the address of a function to get the inverse of a matrix
  setinverse <- function(solve) m <<- solve
  
  # returns m (the inverse)
  getinverse <- function() m
  
  # return the list
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # do we have an inverse of x?
  m <- x$getinverse()
  
  if(!is.null(m)) {
    # yes, we do
    message("getting cached data")
    
    # and here we return it
    return(m)
  }
  
  # if not, lets get the matrix from the object
  data <- x$get()
  
  # compute the inverse
  m <- solve(data, ...)
  
  # cache the inverse in the "matrix"
  x$setinverse(m)
  
  # and, finally, return the inverse
  m
}