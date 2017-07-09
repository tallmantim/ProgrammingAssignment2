## Coursera R Programming - Assignment 2
##
## Tim Jones, tallmantim@gmail.com
##
## These functions provide the capacity to cache results of the solve() function.
## Through use of these functions, if a value has already been calculated for a 
## particular matrix, the result is not re-calculated, rather it is recovered 
## from memory.

## Function : makeCacheMatrix
##
## This function intializes the values of the matrix and sets them in memory,
## allowing them to be recovered through use of the created functions.

makeCacheMatrix <- function(x = matrix()) {
    current_matrix <- NULL
    set <- function(new_value) {
        x <<- new_value
        current_matrix <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) current_matrix <<- solve
    getsolve <- function() current_matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    }


## Function : cacheSolve
##
## This function returns the inverse of the matrix that has been initialized with
## the makeCacheMatrix() function.  It does this through the solve() function. If
## the result has already been calculated the function will return the cached 
## matrix and provide a screen message that it is using the cache version.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    current_matrix <- x$getsolve()
    if(!is.null(current_matrix)) {
        message("getting cached data")
        return(current_matrix)
    }
    data <- x$get()
    current_matrix <- solve(data, ...)
    x$setsolve(current_matrix)
    current_matrix
    }
