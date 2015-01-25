# Caching the Inverse of a Matrix 

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the mean

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


## cacheSolve is a function that returns the inverse of the 
## matrix. It first checks if the inverse has already been 
## computed. If so, it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache 
## through the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(m)
    m
}
