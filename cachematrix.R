## Put comments here that give an overall description of what your
## functions do

## The function named "makeCacheMatrix" will ...
## 1. Set the value of the matrix.
## 2. Get the value of the matrix.
## 3. Set the inverse of the matrix.
## 4. Get the inverse of the matrix.
## Note: this function is almost identical to
##       the "makeVector" example used in the
##       example substituting "mean" for "inverse".

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


## The "cacheSolve" function creates the inverse
## matrix above. It first checks to see if the 
## inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the
## inverse in the cache via the setinverse function.
## Note: Again similar to the example ("cachemean").

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