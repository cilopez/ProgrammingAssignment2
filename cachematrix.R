
# Saves the vector of the matrix
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


# Computes the value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#  amatrix <- makeCacheMatrix(matrix(1:4,2))
#  amatrix$get()               # retrieve the value of x
#  amatrix$getinverse           #retrieve the value of m, which should be NULL
#  cacheSolve(amatrix)     
#  amatrix$getsolve()           # retrieve it directly, now that it has been cached
