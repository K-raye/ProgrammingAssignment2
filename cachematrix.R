## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y     ## save matrix to this function environment
                i <<- NULL  ## revoke cached inverse
        }
        get <- function() x # return matrix
        setinverse <- function(inverse) i <<- inverse # cache inverse
        getinverse <- function() i  # return cached inverse, if any
        
        # return list of functions for working with underlying matrix and its inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                # if cached inverse occurs, then return it
                message("returning cached inverse")
                return(i)
        }
        data <- x$get()        ## extract the underlying data
        i <- solve(data, ...)  ## calculate the inverse
        x$setinverse(i)        ## cache the inverse for use again
        i                      ## return a matrix that is the inverse of 'x'
}