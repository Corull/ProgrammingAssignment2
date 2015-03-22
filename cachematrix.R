## These functions calculate and cache the inverse of a matrix.

## Takes a matrix object 'x' and creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    xinv <- NULL
    set <- function(y) {
        
        x <<- y
        xinv <<- NULL
    }
    
    get <- function() x
    setinverse <- solve(inverse) xinv <<- inverse
    getinverse <- function() xinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Returns the inverse of the matrix 'x'. Uses cached results from 'makeCacheMatrix' if available.

cacheSolve <- function(x, ...) {

    xinv <- x$getinverse()
    if(!is.null(xinv)) {
        
        message("getting cached inverse")
        return(xinv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(xinv)
    xinv
    
}