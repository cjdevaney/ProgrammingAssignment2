## Chris de Vaney 25/05/2014
## These functions create specialist matrix structures as an S3 "class", which allow
## for the creation of new instances of the matrix and for the automated calculation and
## retreival of the matrix inverse. 

## x <- makeCacheMatrix()
## Create a new matrix class instance, with instance variables for the matrix (m) and its inverse(s).
## Example:
##
## grimm <- matrix( c(1,3,5,7), nrow = 2, ncol = 2)
## tst2 <- makeCacheMatrix()
## tst2$set(grimm)
## tst2$get()
##       [,1] [,2]
## [1,]    1    5
## [2,]    3    7
## tst2$setCacheMatrix()
## tst2$getCacheMatrix()
##       [,1]   [,2]
## [1,] -0.875  0.625
## [2,]  0.375 -0.125


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCacheMatrix <- function() s <<- solve(x)
  getCacheMatrix <- function() s
  list(set = set, get = get,
       setCacheMatrix = setCacheMatrix,
       getCacheMatrix = getCacheMatrix)
}

## cacheSolve(aCacheMatrix)
## Return the matrix that is the inverse of the input argument - either calculated or retrieved from cache if it's already been calculated before
## Example:
## cacheSolve(tst2)
## getting cached matrix
##        [,1]   [,2]
## [1,] -0.875  0.625
## [2,]  0.375 -0.125


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getCacheMatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCacheMatrix(m)
  m
}
