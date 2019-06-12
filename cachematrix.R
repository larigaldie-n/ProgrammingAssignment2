## Set of functions to create an object encapsulating a Matrix, and to calculate its inverse
## Keeps the inverse calculation in cache to avoid re-doing the calculation if needed several times

## "makeCacheMatrix" creates the object encapsulating a matrix passed as an argument

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL          ## When the matrix is originally created, its inverse is set to NULL(computed on 1st demand)
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


## "cacheSolve" computes the inverse of the matrix in the object, stores the result in its cache, and returns it

cacheSolve <- function(x, ...)
{
  inv <- x$getInv()
  if (!is.null(inv))           ## If inverse has already been computed before, it is stored in cache
  {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)                ## Store the inverse in cache
  inv
}
