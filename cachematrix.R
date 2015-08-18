## Put comments here that give an overall description of what your
## functions do

## Create helper functions used to store the current value
## and the inverse of the value
makeCacheMatrix <- function(val = matrix()) {
  ## set the inverse value to null
  inv <- NULL
  
  ## used to set the current value
  set <- function(y) {
    val <<- y
    inv <<- NULL
  }
  
  ## return the current value
  get <- function() val
  
  ## set the inverse value
  setinverse <- function(inverse) inv <<- inverse
  
  ##return the inverse value
  getinverse <- function() inv
  
  ## provide access to all the functions
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## get the inverse of the matrix
## if the value is null, then calculate the inverse
## cache the inverse value
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## Attempt to retrive the inverse
  inv <- x$getinverse()
  
  ## If the cached value exists, return cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if the inverse doesn't exist, calculate the inverse, cache the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
