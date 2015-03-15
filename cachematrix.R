## These functions create an object that chaches the inverse value of a matrix.
## If an inverse value exsits in the cache, that value is used. If not, the
## inverse of the matrix is calculated and stored in the cache.

## This function creates a matrix object that stores the matrix and the inverse 
## of the matrix (inv). The function contains "methods" stored in a list for 
## getting and setting the value of the matrix and the inverse value in the 
## cache. Beacuse the getters and setters are functions within in the
## "makeCacheMatrix" function, use the <<- assignment operator in the methods to
## assign the inverse in the environment of the top level function/object

makeCacheMatrix <- function(x = matrix()) {
    ## initialize cached value, similar to an instance varialbe of the cache
    ## matrix object
    inverse <- NULL 
    
    ## setter (to set matrix and initialize the inverse to null)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## getter returns the matrix
    get <- function() x
    
    ## sets the value of the inverse in the cache in the makeCacheMatrix environment
    setinverse <- function(inv) inverse <<- inv
    
    ## returns the value of the inverse that is stored in the caches
    getinverse <- function() inverse
    
    ## creates a list of the methods defined in this object so can call the methods
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks if there is a cached value of the matrix inverse. If
## there isn't, it retrieves the matrix, calculates the inverse, and stores the
## value in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get the value of the inverse in the cache
    inverse <- x$getinverse()
    
    ## if the inverse is not null then it exists in the cahce, use that value
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## get here in code if inverse in cache is null (has not been calculated and stored)
    
    ## get the matrix
    data <- x$get()
    
    ## calculate the inverse of the matrix per the assingment - assume the
    ## matrix is always invertible so don't worry about error handling
    inv <- solve(data, ...)
    
    ## set the matrix inverse into the cache
    x$setinverse(inv)
    
    ## return the value of the inverse
    inv
}
