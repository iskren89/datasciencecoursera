## With these R functions we can efficiently calculate the inverse of a matrix
## If a given matrix does not change and we need to call its inverse
## repeatedly, for example in a loop, it is more efficient to store the 
## inverse matrix in cache rather than to recompute it every time.

## The function makeCacheMatrix creates a special matrix, or more precisely a 
## list of functions that: set and get the value of a matrix and 
## set and get its inverse matrix. 

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


## The cacheSolve function calculates the inverse of the matrix created using
## the makeCacheMatrix function above. The main purpose of the function is to 
## be more efficient when we need to calculate the inverse matrix repeatedly. 
## The cacheSolve function first checks whether the inverse matrix has already
## been calculated - in this case, the inverse matrix is retrieved from cache
## rather than calculated again. If it is the first time that the inverse is
## being calculated, the function calculates the inverse matrix and stores
## that inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
  }