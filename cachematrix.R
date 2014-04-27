## Assignment: Caching the Inverse of a Matrix
## To save the computation cost associated with inversion computation of large matrices,
## following functions are made. These functions use the <<- operator in R to cache the values from
## different environments, thereby allowing caching of previously calculated values. Using such caching
## we can avoid the computation cost associated with repeated calculations.


## This function creates a special "matrix" object, which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. Basically, if the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
