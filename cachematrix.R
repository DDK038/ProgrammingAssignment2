#store the function of inverse into makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
#set "inv" to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
#method to get the inverse and computer it using solve
#return the inverse function
#sets the cached inverse and retrieve the cached inverse
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() {
    inv <- solve(x)
    inv
  }
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

#return the computed inverse if available and return the function
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
