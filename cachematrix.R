## makeCacheMatrix stores a matrix object and then cache´s its 
## inverse, following cacheSolve calculates the inverse of the
## matrix object and sets the inverse via the setinverse function.


## This function cache's the inverse of the matrix object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Then cacheSolve see's if the inverse exists if not calculates
## the inverse of the matrix and sets the value of it.

cacheSolve <- function(x, ...) {
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
