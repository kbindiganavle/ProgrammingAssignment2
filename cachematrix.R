## R script containing functions to both find an inverse of a given square matrix
## and cache the result to avoid recomputing the inverse if the matrix remains unchanged

## function that provides getters and setters for both the passed in square matrix
## data as well as the computed "inverse matrix" result
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


## function that either retrieves cached "inverse matrix" data from the
## given "makeCacheMatrix" object or if the cache's empty compute the
## inverse and cache it in the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("computing the inverse for the first time")
    data <- x$get()
    m <- solve(data, ...)
    message("caching the data for future use")
    x$setsolve(m)
    m
}
