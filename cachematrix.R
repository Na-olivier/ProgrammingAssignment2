## These functions create a special matrix object that stores a matrix
## and caches its inverse so that repeated inverse calculations can be avoided.

## Create a special matrix object that can store its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) {
                m <<- inv
        }
        
        getinv <- function() m
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Return the inverse of the special matrix, using the cached value if available.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
