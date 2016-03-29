## The first function caches the data and the result of the function that is calculated in the second function.
## The second function calculates the function solve and uses the first one to cache the result of the solve function in case it is not yet cached.


## This function receives a matrix object and caches it in a variable of the environment. It caches the solve (inverse) of the matrix as well as long as the
## setsolve function is invoked.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function receives a matrix and, in case it is not yet cached, it returns the inverse of that matrix.

cachesolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}