## Two functions to cache the inverse of a matrix

## This function is to creating matrix object to cache inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(matrix){
    x <<- matrix
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse)
    i <<- inverse
  
  getInverse <- function()
    i
  
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function is to compute the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    if(!is.null(i)){
      message("Getting cached data")
      return(i)
    }
    
    data <- x$get()
    
    i <- solve(data) %*% data
    
    x$setInverse(i)
    
    i
}

