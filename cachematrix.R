# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

# Matrix inversion is usually a costly computation and there may be some benefit
# It is easier to caching the inverse rather than  compute it repeadtedly.
# This function will be used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMAT <- NULL
  setMAT <- function(y) {
    x <<- y
    invMAT <<- NULL
  }
  get <- function() x
  set_INV <- function(inverse) invMAT <<- inverse
  getINV <- function() invMAT
  list(setMAT=setMAT, get=get, set_INV=set_INV, getINV=getINV)
}


# The function returns the inverse of the matrix. 
# By caching, it checks either the inverse has already been computed. 
# 1) IF SO, it gets the result and skips the computation. 
# 2) IF NOT, it computes the inverse, sets the value in the cache via setINV function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  invMAT <- x$getINV()
  if(!is.null(invMAT)) {
    message("....getting cached data...")
    return(invMAT)
  }
  data <- x$get()
  invMAT <- solve(data)
  x$set_INV(invMAT)
  invMAT
}

## Sample run:
x = rbind(c(2, -3/4), c(-3/4, 2))
m = makeCacheMatrix(x)
m$get()
##        [,1]  [,2]
##  [1,]  2.00 -0.75
##  [2,] -0.75  2.00

## No cache in the first run

cacheSolve(m)
##           [,1]      [,2]
##  [1,] 0.5818182 0.2181818
##  [2,] 0.2181818 0.581818

## Retrieving from the cache in the second run

cacheSolve(m)
## ...getting cached data...
##              [,1]      [,2]
##  [1,] 0.5818182 0.2181818
##  [2,] 0.2181818 0.5818182