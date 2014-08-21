## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        set           <- function(y) {
                x             <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) matrixInverse <<- solve
        getSolve <- function() matrixInverse
        list(set = set, get = get,
        setSolve = setSolve,
        getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getSolve()
        if(!is.null(matrixInverse)) {
                message("matrix inverse fetched from cache data")
		#message("getting cached data")
                return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data, ...)
        x$setSolve(matrixInverse)
        matrixInverse
}
