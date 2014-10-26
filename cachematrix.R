## These two functions work in pair to calculate, cache and retrieve
## the inverse of a matrix

## makeMatrix is given a matrix and returns a list consisting of functions
## to set and get both the given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve is a function which expects as an argument a list returned
## by makeCacheMatrix, tries to find a cached version of the inverse of this
## matrix, and in case of a cache miss calculate the inverse matrix, set it
## in the cache for further use, and returns it

cacheSolve <- function(x, ...) {
    
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
