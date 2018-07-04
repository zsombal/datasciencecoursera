## Put comments here that give an overall description of what your
## functions do

  ## Write a short comment describing this function
# creates an environment that stores a matrix and caches its mean
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# calculates the inverse of the special environment created with makeCacheMatrix().
# First it checks if the inverse has already been calculated

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

# Example data that shows that it works

aa=matrix(data=c(1,2,3,4),nrow=2)
yy<-makeCacheMatrix(aa)
cacheSolve(yy)
yy$getinverse()



