
## Create a cache inverse of a matrix with 4 functions to get or set that matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## If cache exists return it. Otherwise, calculate the inverse by using solve()
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)){
    return (m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
