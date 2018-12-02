## Two functions are defined:
## 'makeCacheMatrix' returns a custom matrix that allows to cache the inverse, 
## such that the inverse needs to be computed only once.
## 'cacheSolve' returns the inverse of the custom matrix that is returned by
## makeCacheMatrix.

## makeCacheMatrix <- function(x = matrix())
## Return a custom matrix that allows to cache the inverse, such that the 
## inverse needs to be computed only once. 
## Input:
## - x: a square matrix.
## Output:
## - The output is a list of four functions:
##   - set(x = matrix()): The matrix can be (re)set.
##   - get(): The actual matrix is returned.
##   - setinverse(y = matrix()): The inverse of the actual matrix is set.
##   - getinverse(): The inverse of the actual matrix is returned if it is set.
##       otherwise NULL is returned.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse as an empty matrix, meaning that it isn't computed.
    inv <- NULL
    
    # The set function (re)sets the actual matrix. As a result, the inverse that
    # is potentially computed is of no use, so the inverse will be set to NULL.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # The get function simply returns the actual matrix.
    get <- function() x
    
    # With setinverse(inverse), the inverse of the matrix can be set.
    # Here, it is assumed that the inverse that is passed is correct.
    setinverse <- function(inverse) inv <<- inverse
    
    # With getinverse, the matrix inverse is obtained. If the inverse is not
    # set, NULL will be returned.
    getinverse <- function() inv
    
    # Output of this function are the four above defined functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve <- function(x, ...)
## Return the inverse of the matrix returned by the makeCacheMatrix.
## If the inverse is already computed before, that inverse will be used. The
## 'solve' function is used to compute the inverse. Any extra arguments provided
## with the ... are also passed to the 'solve' function. 
cacheSolve <- function(x, ...) {
    # Obtain the matrix inverse from the 'special' matrix returned by
    # makeCacheMatrix.
    inv <- x$getinverse()
    
    # If the inverse is not NULL, it means that it is computed, so we can return
    # the result.
    if (!is.null(inv)) {
        message("Getting cached matrix inverse.")
        return(inv)
    }
    
    # Apparantly, the inverise NULL, which means that the inverse is not yet 
    # computed. Hence, if this is the case, we will need to compute it and store
    # it, such that we do not need to do it again.
    mat = x$get()           # Obtain the matrix.
    inv <- solve(mat, ...)  # Compute the inverse.
    x$setinverse(inv)       # Set the inverse for later use.
    
    # Return the computed inverse.
    inv
}
