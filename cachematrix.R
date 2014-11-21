## Matrix inversion is a costly mathematical operation, which for n x n matrix
## could have O(n^3) complexity. These 2 functions when used together, could
## save a considerable time in case of computing matrix inversion repeatedly 
## (e.g. in a loop), by caching the value of the matrix inversion.


## makeCacheMatrix:
## 	creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 	# This is the variable that saves the value of matrix 
			# inversion. Reset to NULL when makeCacheMatrix is called. 	
	set <- function(y) { # Takes an input matrix y.
		x <<- y 	# Saves the value of the input matrix
		inv <<- NULL 	# Reset the value of matrix inversion 
	}
	get <- function() x  		# Simply return the original matrix value
	setinverse <- function(inverse) {
		inv <<- inverse 	# Set the value of matrix inversion
	}
	getinverse <- function() inv 	# Return the value of matrix inversion
	list(set = set, get = get, 
	     setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve:
##	computes the inverse of the special "matrix" returned by makeCacheMatrix 
##	above. If the inverse has already been calculated (and the matrix has not
##	changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data) 	# Compute the inverse of the matrix
	x$setinverse(inv)
	inv
}
