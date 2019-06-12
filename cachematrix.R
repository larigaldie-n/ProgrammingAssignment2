## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          ## When the matrix is originally created, its inverse is set to NULL
  set <- function(y)   ## If we change what the matrix is, we also set back its inverse to NULL
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ## Simply returns the matrix itself
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv))
  {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
