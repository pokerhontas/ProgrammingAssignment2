## Coursera Programming Assignment 2

## Last Edited on 20/06/15

## Function to create a special "matrix" object
## that can cache the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}

## Function that computes the inverse of the special
## "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getMatrix()
    if(!is.null(m)){
        message("Getting cached data.")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setMatrix(m)
    m
}
