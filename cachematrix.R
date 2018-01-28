## our goal is to write a pair of functions that cache the inverse of a matrix.
 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    # this function is a list containning a function 
    # to set and get the value of the matrix and 
    # to set and get the inverse of the matrix

    m <- NULL
	

	#set the value of the matrix x with the matrix y
    set <- function(y) {
        x <<- y
        m <<- NULL
    }


	#get the value of the matrix x
    get <- function() x


	#set the value of the inverse of the matrix using the solve function
    setinverse <- function(solve) m <<- solve

	#get the value of the inverse
    getinverse <- function() m

	#return a list of the 4 functions wrote above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()

	# we checks to see if the inverse has already been calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
	

	# the inverse has not been calculated, so we set the inverse using 
	# the solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
