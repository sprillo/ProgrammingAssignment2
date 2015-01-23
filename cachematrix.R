## makeCacheMatrix is a function taking a matrix x as a parameter.
## The returned object x_wrapped encapsulates a matrix, initially equal to x,
## and supports the following methods for public use:
## - set(y): To set the underlying matrix to be equal to y.
## - get(): Returns the underlying matrix.
## The returned object x_wrapped is meant to provide inverse matrix caching
## for the underlying matrix x via the cacheSolve function listed below. In
## this regard, the object x_wrapped returned by makeCacheMatrix supports the
## methods setinverse(inv) and getinverse(), but should NOT be used publicly.
## They are meant to be called only by the cacheSolve function. Do not use
## them unless you know what you are doing.

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) x_inverse <<- inv
    getinverse <- function() x_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function intended to work in conjunction with the object
## x_wrapped returned by makeCacheMatrix in order to provide inverse matrix
## caching.
## cacheSolve takes an object x_wrapped returned by the makeCacheMatrix (which
## encapsulates a matrix x), and returns the inverse of the underlying matrix
## x. It is designed in order to avoid recomputing the inverse of x if it
## has already been calculated.
## Aditional parameters of the solve(a,b,...) function can be passed through
## cacheSolve if desired.
## The function will print a message if the inverse of the matrix is found
## to be cached. Consider removing this line for efficiency purposes.
##
## Check further below for an example on how to use makeCacheMatrix and 
## cacheSolve.

cacheSolve <- function(x_wrapped, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x_wrapped$getinverse()
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    x <- x_wrapped$get()
    x_inverse <- solve(a = x, b = diag(nrow(x)),...)
    x_wrapped$setinverse(x_inverse)
    x_inverse
}

## Example of use of makeCacheMatrix and cacheSolve in the prompt:
##
## > x <- matrix(c(1,2,3,4),2,2)
## > x_wrapped <- makeCacheMatrix(x)
## > x_inverse <- cacheSolve(x_wrapped)
## > x_inverse
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > x_inverse <- cacheSolve(x_wrapped)
## getting cached data
## > x_inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > x_wrapped$set(matrix(c(2,3,4,5),2,2))
## > x_inverse <- cacheSolve(x_wrapped)
## > x_inverse
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > x_inverse <- cacheSolve(x_wrapped)
## getting cached data
## > x_inverse
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
