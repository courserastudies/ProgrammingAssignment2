## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## The following pair of functions  cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Assumption. The matrix given supplied is always a square invertible matrix
  ## return: create and return a special "vector", really a list containing functions (get, set, setinv, getinv)
  inv = NULL
  set = function(y) {
    x <<- y # Leverage lexical scoping rules. i.e. using `<<-` 
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  if (!is.null(inv)){
    # if the inverse has already been calculated
    # get it from  cache and skip the computation.
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv) 
  return(inv)
}
