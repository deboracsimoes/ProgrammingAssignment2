## CACHING THE INVERSE OF A MATRIX:
## This pair of functions creates, stores and get a matrix and itS inverse in/from cache
## in order to save some time in computer processing.

## The makeCacheMatrix function creates a special "matrix" object and cache its inverse.
## It contains a list of four functions to:
##      1. Set the matrix and store it in cache (set)
##      2. Get the matrix from cache (get)
##      3. Set the inverse matrix and store it in cache (setInverse)
##      4. Get the inverse matrix from cache (getInverse)

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y){
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) x_inv<<- inv
        getInverse <- function() x_inv
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix created with the
## above function. However, it first checks if the matrix has already
## been inversed. If so, it gets the inversion matrix from the cache,
## skipping computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getInverse()
        if (!is.null(x_inv)){
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data,...)
        x$setInverse(x_inv)
        x_inv
}
