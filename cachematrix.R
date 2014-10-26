##
##  The following two functions solve the inversion of a matrix
##  then cache the result to a global variable which will be called
##  if the calculation is attempted again on an identical matrix
##

## makeCacheMatrix caches the result of inversing the matrix to
## the global variable x, as well as makes $getsolve and $setsolve 
## accessible to other functions outside of makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns a matrix that is the inverse of argument 'x'
## if the result exists already (due to the caching operation) 
## return the result, else calculate the matrix inversion for the
## first time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}



