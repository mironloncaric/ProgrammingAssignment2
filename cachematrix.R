## Since computing inverse of a matrix is resource heavy, it would
## be beneficial to cache the inverse of a given matrix in order to
## save time and resources required for multiple computations of the
## same matrix

## The following function returns a list of functions:
## set - function "set" sets the matrix which is to be inverted. The
##       matrix is passed down as a function argument.
## get - function "get" returns the matrix which is to be inverted.
## setinverse - function "setinverse" caches the calculated inversed
##              matrix by storing it in memory in the variable "inv_cache"
## getinverse - function returns the calculated inversed matrix stored
##              in the "inv_cache" variable.

makeCacheMatrix <- function(x = matrix()) {
  inv_cache <- NULL
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inv_cache <<- inverse_matrix
  getinverse <- function() inv_cache
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## The following function calculates the required inverse matrix,
## stores it into the object created by the "makeCacheMatrix" function
## via the "setinverse" function and finally returns it.

cacheSolve <- function(x, ...) {
  
  ## Upon the first time calculation of the matrix, the return value of the 
  ## "getinverse" function will be NULL. In that case, the inverse will be 
  ## calculated, and then stored in memory.
  
  inv_cache <- x$getinverse()
  if (!is.null(inv_cache)) {
    
    ## If the value of the variable "inv_cache" is not NULL, that means
    ## that the inverse has already been calculated and should be fetched
    ## from memory.
    
    message("getting cached data")
    return(inv_cache)
  }
  
  ## The inverse of an invertable matrix can be found using the "solve"
  ## function built into R.
  data <- x$get()
  inv_cache <- solve(data)
  x$setinverse(inv_cache)
  
  ## The inverse matrix is returned.
  inv_cache
}
