#Assignment: Caching the Inverse of a Matrix

# 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  inputset <- function(y) {                           ## set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  inputget <- function() x                            ## get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse     ## set the value of the matrix inversion
  getinverse <- function() inv                        ## get the value of the matrix inversion
  list(set=inputset, get=inputget, setinverse=setinverse, getinverse=getinverse)
  
}

# 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Look for cached inversion first and use it if found, otherwise it
## calculates the inversion. 
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()                               ## Check if cached value exists
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
