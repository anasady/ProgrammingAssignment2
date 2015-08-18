## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create helper functions used to store the current value
## and the inverse of the value
makeCacheMatrix <- function(val = matrix()) {
  inv <- NULL
  set <- function(y) {
    val <<- y
    inv <<- NULL
  }
  get <- function() val
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function
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
