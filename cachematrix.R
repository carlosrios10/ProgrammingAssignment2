## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## These functions are able to cache potentially time-consuming computations.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) sol <<- solve
        getSolve <- function() sol
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getSolve()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setSolve(sol)
        sol
}
