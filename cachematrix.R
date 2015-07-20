## Below are two functions that are used to create an object that will allow us to
## store a matrix and cache its inverse


## This first function creates a matrix-like object with additional functions
## that allow you to get/set the input matrix and get/set the matrix inverse
## (please note the function assumes the matrix is in fact invertible)

makeCacheMatrix <- function(x = matrix()) {
    ## initialise inverse to NULL
    inv <- NULL
    
    ## setter function to store matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## getter function to return the matrix
    get <- function() x
    
    ## setter function for the inverse matrix
    set_inv <- function(inverse) inv <<- inverse
    
    ## getter function for the inverse matrix
    get_inv <- function() inv
    
    ## return list of methods
    list(set = set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## This second function calculates the inverse of the matrix-like object
## created by the function above. It will first check to see if the inverse
## has been calculated before and return the cached value if available.

cacheSolve <- function(x, ...) {
    
    ## retrieve cached inverse (which may not have been calculted yet)
    inv <- x$get_inv()
    
    ## check if cached inverse exists and return if available
    if (!is.null(inv)){
        message("Getting Cached Data")
        return (inv)
    }
    
    ## if cache doesn't exist, get the matrix
    data <- x$get()
    
    ## calculate inverse
    inv <- solve(data)
    
    ## cache the inverse
    x$set_inv(inv)
    
    ## return the inverse
    inv
}
