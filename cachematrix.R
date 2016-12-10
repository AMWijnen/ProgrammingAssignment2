## For this second programming assignment of the Coursera course in R programming I have written two functions:
## "makeCacheMatrix" and "cacheSolve" to cache the inverse of a matrix.

## The function "makeCacheMatrix" creates a matrix that can cache its reserve.  

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
			set <- function(y) {
				x <<- y
				inv <<- NULL
			}
			get <- function() x
			setinv <- function(inverse) inv <<- inverse
			getinv <- function() inv
			list(set = set, get = get, 
				setinv = setinv,
			 	getinv = getinv)
}


## The "cacheSolve" function computes the inverse of the special matrix object that is returned from the "makeCacheMatrix" 
## function. When the inverse is already calculated (and the matrix hasn't changed), then "cacheSolve" retrieves the inverse
## of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
                return(inv)
        }
        data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv        
}
