## This file contains two functions. The first function, makeCacheMatrix, takes
## in a matrix and returns a list of functions which can thereafter be used to
## get or set the matrix object, or get or set the inverse of the matrix object.
## The second function will solve for the inverse of the special matrix object
## created by the first function - or if the inverse has already been solved,
## it will fetch that inverse from a cached variable.

## makeCacheMatrix takes in a matrix and returns a list of functions which can
## thereafter be used to get or set the matrix object, or get or set the inverse
## of the matrix object. Note: this makes no check that the matrix is square
## (and therefore invertible).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will solve for the inverse of the special matrix object
## created by the first function - or if the inverse has already been solved,
## it will fetch that inverse from a cached variable. Note: there is no check
## to guarantee that the matrix is square (and therefore invertible).

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
