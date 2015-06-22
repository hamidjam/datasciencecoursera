## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv_mtrx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtrx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_mtrx <<- inv 
  getinv <- function() inv_mtrx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mtrx <- x$getinv()
  if(!is.null(inv_mtrx)) {
    message("getting cached data")
    return(inv_mtrx)
  }
  data <- x$get()
  inv_mtrx <- solve(data, ...)
  x$setinv(inv_mtrx)
  inv_mtrx
}
