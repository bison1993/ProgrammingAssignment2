## Programming Assignment 2 - R Programming on Coursera
##
## Provided below is a pair of functions that provide the functionality
## to create a special matrix object that can cache its inverse.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinvmat <- function(invm) invmat <<- invm
    getinvmat <- function() invmat
    
    list(set = set, get = get, 
         setinvmat = setinvmat, getinvmat = getinvmat)
}


## This function will calculate the inverse of the special matrix.
## But, if the inverse has already been cached, it will pull it from the cache rather than recompute.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinvmat()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinvmat(im)
    im
}
