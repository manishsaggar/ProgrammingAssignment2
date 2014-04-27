## Assignment: Caching the Inverse of a Matrix
## To save the computation cost associated with inversion computation of 
## large matrices,following functions are made. These functions use the 
## <<- operator in R to cache the values from different environments, 
## thereby allowing caching of previously calculated values. Using such caching
## we can avoid the computation cost associated with repeated calculations.


## This function creates a special "matrix" object, which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initializing the inverse as NULL  
  inv <- NULL
  
  # creating a function to set the value of x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # creating a function to extract the value of x
  get <- function() x
  
  # creating a function to set the inverse value of x
  setInverse <- function(inverse) inv <<- inverse
  
  # creating a function to get the inverse value of x 
  getInverse <- function() inv
  
  # returning the newly created list from x that contains all the get and 
  # set functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. Basically, if the inverse has already been 
## calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # get the inverse value from x
  inv <- x$getInverse()
  
  # check and return, if cached inverse is available (i.e, not NULL)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # since inverse is not cached, lets calculate it using function solve()
  # NOTE: need to use the get() function of x, as it is a list and we can't
  #       pass a list to R's inbuilt solve() function
  data <- x$get()
  inv <- solve(data, ...)
  
  # setting the inverse to our x variable, i.e., caching its value for later
  # use.
  x$setInverse(inv)
  
  # returning inverse
  inv
}
