## makeCacheMatrix takes as an argument the matrix x, defines the variable inv to be null and creates 
## set and get methods for both x and the inverse of x which can be called by other functions. 
## It outputs a list of methods on x.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
     x <<- y
     inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)  inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve take as an argument the matrix defined in makeCacheMatrix and outputs its inverse. 
## If the inverse has not yet been set, it sets it in variable inv and outputs its contents. 
## If the inverse has already been set (i.e. !is.null(inv) evaluates to TRUE) then it returns inv from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
