## makeCacheMatrix() cacheSolve() enable the caching of a matrix and
## its inverse; if the inverse is not cached it will be calculated and
## subsequently cached for later access


## makeCacheMatrix( x )
##
## Creates a container for a matrix, its inverse and the functions to
## get&set these. The functions are:
##	set/stores matrix x
##	gets/retrieves matrix x the value of the vector
##	set the inverse matrix of the matrix x
##	get the inverse matrix of the matrix x
##
## Returns a list containing the four functions.

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL

        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi

        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve( x )
##
## Determines the inverse matrix of matrix x; if the inverse was
## previously calculated it takes the cached version from makeCacheMatrix(),
## otherwise it calculates the inverse.
##
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	mi <- x$getinverse()

        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
	else {
        	data <- x$get()
        	mi <- solve(data, ...)
        	x$setinverse(mi)
        	return(mi)
	}
}
