## makeCacheMatrix is a function that creates a list of functions to
## 1) set the values of the matrix (set)
## 2) get the values of the matrix (get)
## 3) set the values of the matrix inverse (setSolve)
## 4) get the values of the matrix inverse (getSolve)
##
##
## cacheSolve is a function that calculates the inverse of a matrix created through the 'makeCacheMatrix' function;
## it first checks if the matrix inverse has already been calculated for its most recent values (last "state")
## if so, the inverse matrix is retrieved from cache data and returned
## if not, the inverse matrix is calculated, the cache data is updated 


## makeCacheMatrix: function that creates a list of functions to manage (get/set) the contents of a matrix
## that is stored is cache data
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
## cacheSolve is a function that calculates the inverse of a matrix created through the 'makeCacheMatrix' function;
## if the inverse has already been calculated, it is retrieved from cache data, otherwise it is calculated and the
## cache data is updated 
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



#working example:
#source("cachematrix.R")
#
#mat1     <- makeCacheMatrix(matrix(c(2,2,3,2),2,2))
#mat1_inv <- cacheSolve(mat1) # matrix inverse is calculated and then cached
#mat1_inv <- cacheSolve(mat1) # matrix inverse had already been calculated, so it gets fetched from cache data
#
#mat2     <- makeCacheMatrix(matrix(c(4.9685,4.3950,3.2449,4.3950,5.3439,3.4109,3.2449,3.4109,4.2245),3,3))
#mat2_inv <- cacheSolve(mat2) # same as with mat1
#mat2_inv <- cacheSolve(mat2) # same as with mat1
#
#mat3     <- makeCacheMatrix(matrix(c(2.7533,2.2067,2.1660,2.1623,2.2067,3.5203,2.5093,2.6402,2.1660,2.5093,2.6859,2.1274,2.1623,2.6402,2.1274,3.4954),4,4))
#mat3_inv <- cacheSolve(mat3) # same as with mat1
#mat3_inv <- cacheSolve(mat3) # same as with mat1
#
#
#matrices generated using the commands "X = rand(10,n); X = X'*X;" in MATLAB, where 'n' is the matrix dimension (n rows, n columns, n*n elements)
#
