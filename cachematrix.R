## Implement functions to create an object that will store
## matrix data, and will allow computation and caching of
## the inverse of that matrix.
## The cache is invalidated when the matrix data changes

## makeCacheMatrix takes a matrix argument 'x' and 
## returns a 'matrix' like object, which is a list object 
## containing functions to set and get the matrix data
## as well as get and set the solved (inverse) matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        ## invalidate the cache
        s <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve takes an object created by makeCacheMatrix
## as well as any extra parameters to the solve() function
## and returns the inverse of the matrix data.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if (!is.null(s)) {
        ## use the cached value
        message("getting cached data")
        return(s)
    }
    
    ## compute the inverse and cache it
    data <- x$get()
    s <- solve(a, ...)
    x$setsolve(s)
    s
}
