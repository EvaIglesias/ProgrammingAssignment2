
## The first function, makeCacheMatrix creates a special "matrix" that can cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  set <- function(y) {
    x <<- y  
    m <<- NULL  
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function cacheSolve calls the functions stored in the special "matrix" returned by makeCacheMatrix (above). 
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache and print "getting cached data". 
## If the input is new, it calculates the inverse of the data and print it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}
