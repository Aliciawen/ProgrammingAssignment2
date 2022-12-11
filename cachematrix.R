## Calculate the inverse of a matrix.

## Create 4 basic functions for cached matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## When the inverse of a matrix has been cached, return the cached value
## Otherwise, calculate the matrix, cache it and return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse = x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
