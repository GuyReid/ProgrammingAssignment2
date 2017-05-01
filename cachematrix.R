## Functions for caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	##Initialise
         inverse <- NULL
	##Set Matrix
         set <- function(y) {
                 x <<- y
                 inverse <<- NULL
         }
	##Get Matrix
         get <- function() x
      ## Set inverse
        setinverse <- function(inverse) inverse <<- inverse
      ## Get inverse
         getinverse <- function() inverse
         list(set = set, get = get,
            setinverse = setinverse,
             getinverse = getinverse)
 }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  	##Returns a Matrix that is the inverse of x
        inverse <- x$getinverse()
	##Check that Inverse is not empty and Return
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
	##Get Matrix
        data <- x$get()
	##Calculate Inverse
        inverse <- solve(data, ...)
	##Set Inverse
        x$setinverse(inverse)
	##Return Matrix
        return(inverse)
}
