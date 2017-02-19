## These 2 functions show how to implement a solution to cache the inverse of a matrix 
## in order to improve the code performance.

## This function stores a matrix object and its inverse.
## It provides functions to set and get a matrix and set and get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL ## if a new matrix is set, the inverse is cleared!
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix if it has already been stored in
## the makeCacheMatrix function. 
## Otherwise it calculates the inverse of the matrix and it caches it in the 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
