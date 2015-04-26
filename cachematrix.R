## These functions are used to create a cache of a matrix and solve to find the inverse of the cached matrix

# Example of function usage
# Create a matrix
#> mymatrix <- matrix(c(4,3,3,2), nrow=2, ncol=2)
#
# Cache the matrix
#> cm <- makeCacheMatrix(mymatrix)
# 
# get value from cache
#> cm$get()
#
# find inverse of the matrix - first time will do actual solve
#> cacheSolve(cm)
#
# second time will use cache and return message
#> cacheSolve(cm)
#
# now change matrix in cache
#> cm$set(matrix(1:4,nrow=2,ncol=2))
#
# find inverse of the NEW matrix - because is first time will do actual solve
#> cacheSolve(cm)
#
# second time will use cache and return message
#> cacheSolve(cm)


## makeCacheMatrix will store the passed in matrix in a variable in the parent environment
makeCacheMatrix <- function(x = matrix()) {
	## start out with null variable this variable will be used for the cache
	m <- NULL
        # function to set new value for matrix <<- sets in the parent environment, which is the makeCacheMatrix function
        # in this instance
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
        #return originial matrix passed in
	get <- function() x
        #set a value for m, which is the cache
	setsolve <- function(solvedm) m <<- solvedm
        # get current value for m, which is the cache
	getsolve <- function() m
        #list of functions
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)

}


## cacheSolve is used to find the inverse of the matrix cached with makeCacheMatrix
# assumes matrix is invertible
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        # set m as the matrix that was stored
	m <- x$getsolve()
        # if m is not null then return the cached value of m
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
        #since m is null by this point retrieve the matrix and store in data
	data <- x$get()
        #set m as the inverse of the matrix passed in
	m <- solve(data, ...)
	x$setsolve(m)
        # return the resulting inverse matrix
	m
}
