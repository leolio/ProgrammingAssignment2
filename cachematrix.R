## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # the inverse of the matrix
    inv <- NULL
    
    # method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # method to get the matrix
    get <- function() x
    
    # method to set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # method to get the inverse of the matrix
    getinverse <- function() inv
    
    # return the list of the methods
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    # get the inverse of x
    inv <- x$getinverse()
    
    # return the inverse of x if it is computed
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    
    # if it is not computed
    
    # get the matrix
    data <- x$get()
    
    # compute the inverse of the matrix
    inv <- solve(data, ...)
    
    # set the inverse of the matrix
    x$setinverse(inv)
    
    # return the inverse of the matrix
    inv
}
