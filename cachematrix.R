

##makeCacheMatrix returns a list of functions which
## 1. Sets a value for a matrix
## 2. Gets the value for the matrix
## 3. Sets the inverse of the matrix
## 4. Gets the inverse of the matrix


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


## Write a short comment describing this function
## cacheSolve retruns the inverse of the matrix x
## If m is NULL then the inverse has not been calculated before
## If m has a value, then the inverse has been computed previously,
## and the function returns the pre-computed value

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
