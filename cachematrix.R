## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The functions written here caches the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # function for setting values
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # function for getting the matrix
        get <- function() x
        
        # function for setting inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # function for getting inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        # check if inverse is already cached
        inv <- x$getinverse()
        
        # if inverse exists, return it
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # else, compute the inverse, cache it and return it
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
