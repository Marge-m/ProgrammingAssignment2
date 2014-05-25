## These two functions allow not to duplicate calculation of inverted matrix 
## if it was done earlier and put into cache and just take it from cache. 


## The function "makeCacheMatrix" creates a list with functions "set" - to set 
## value of matrix x, "get" - to get value of matrix x, "setsolve" - to set the 
## value of inverted matrix, "getsolve" - to get the value of inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function "cacheSolve" checks if cached value of inverted matrix of x 
## already exists and returns it. Otherwise, it performs calculations of inverted
## matrix and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  ## Return a matrix that is the inverse of 'x'
}
