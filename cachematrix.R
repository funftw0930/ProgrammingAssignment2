## This set of functions calculates the inverse of
## matrices. If the inverse of the matrix has already
## been calculated, it returns the cached result, 
## instead of recalculating.

## Creates an object that caches a matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix) {
        x <<- matrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(imatrix) inverse <<- imatrix
    getInverse <- function() inverse
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse given a cacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    inputMatrix <- x$get()
    inverse <- solve(inputMatrix)
    x$setInverse(inverse)
    inverse
}
