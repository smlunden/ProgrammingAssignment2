## Set of functions that will return a matrix's inverse.
## If the inverse exists in cache, the cached inverse will be returned.
## If the inverse does not exist in cache, the inverse will be 
## 	calculated, set in cache, and returned.

## Create a list of get/set functions to cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	# set empty inv
	inv <- NULL
	# set matrix in cache
	setMatrix <- function(y) {
		x <<- y
		inv <<- NULL
	}
	# get matrix from cache
	getMatrix <- function() {
		x
	}
	# set inverse of matrix in cache
	setInverse <- function(inverse) {
		inv <<- inverse
	}
	# get inverse of matrix from cache
	getInverse <- function() {
		inv
	}
	# create list of functions
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Checks for existence of a cached inverse.
##   Returns either the cached version or a newly computed inverse.
cacheSolve <- function(x) {
	# check cache for inverse
	inv <- x$getInverse()
	# if inverse exists, set message and return cached inverse
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	
	# if the inverse is not cached... 
	# get the matrix back
	thisMatrix <- x$getMatrix()
	# calculate the inverse of the matrix
	inv <- solve(thisMatrix)
	# set the new inverse in cache
	x$setInverse(inv)
	# return new inverse
	inv
}
