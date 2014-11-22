## Functions to perform caching of matrix inversion to save computation for similar matrix.
## Customize matrix object is created to cache invert matrix. Please see below:

## makeCacheMatrix is used to create cache invert matrix object. 

makeCacheMatrix <- function(x = matrix()) {
	# Define set and get methods for inverse.
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Calculate the inverse of matrix if there is no inverse matrix exist in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
