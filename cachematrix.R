##These functions cache potentially time-consumimg computations of the inverse matrix.

## This function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
        inv <- NULL
        set <- function(k) {
                mtx <<- k
                inv <<- NULL
        }
        get <- function() mtx
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function, cacheSolve, calculates the inverse of the matrix created with the makeCacheMatrix.  

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'mtx'
        inv <- mtx$getInverse()
        if(!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }
        dat <- mtx$get()
        inv <- solve(dat, ...)
        mtx$setInverse(inv)
        inv
}
