## The function below creates the special 'matrix' object
## and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## create special 'matrix' object
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  ## set or get inverse of x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  ## create the object itself
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function below calculates the inverse of a matrix
## If the inverse for the given matrix has been already calculated (cached)
## then it is returned from cache without making calculations again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## retrieve data from cache - already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## no data in cache - calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
