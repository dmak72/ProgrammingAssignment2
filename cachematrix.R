#####################################
## The two functions below are used to create a special object that stores a numeric matrix and cache's its inverse.
## Inverse calculations are sometimes computationally intensive so caching the result is an efficient way to obtain
## these values.
#####################################

#####################################
## Name: makeCacheMatrix
## Purpose:
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
#####################################
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#####################################
## Name: cacheSolve
## Purpose:
## calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the inverse 
## in the cache via the getinverse function.
#####################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
