## The following functions create a special "matrix" object that stores
## a given matrix and will also store/cache the inverse of the matrix.
## NOTE The file "HW_2 Sample Run" provides examples of usage.

## makeCacheMatrix: This function accepts a matrix as an argument
## and creates a special "matrix" object that can cache its inverse.
## The inverse is set to NULL when a new object is created or whenever
## the matrix that is stored is updated using the set() method. It also
## provides a get() method that returns the value of the matrix, and the
## methods setinv() and getinv() that set or get the inverse of the matrix.

## Here are two ways to create a special "matrix"
##
##   plainMatrix1 <- matrix(c(3,2,0,0,0,1,2,-2,1), nrow=3, ncol=3)
##   specialMatrix1 <- makeCacheMatrix(plainMatrix1)
##
##   specialMatrix2 <- makeCacheMatrix(matrix(rnorm(25), c(5,5)))

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: This function accepts a special "matrix" as an argument.
## It returns the inverse of the special "matrix".  If the inverse has
## already been calculated (and the matrix has not changed), then this
## method will return the value of inverse that is stored in cache,
## otherwise it will calculate the inverse and store it in cache.

## For example:
## cacheSolve(specialMatrix1) returns the inverse of specialMatrix1

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## If the inverse exists in cache return it
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting and using cached data")
        return(inv)
    }
    ## If inv is NULL (cache does exist) then
    ## calculate, store and return the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv  
}
