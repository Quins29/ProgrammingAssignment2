## This function create a special "matrix" object that can catche its inverse
## we set and get the value of the cache and the solve.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        setcache <- function(y) {
                x <<- y
                cache <<- NULL
        }
        getcache <- function() x
        setsolve <- function(solve) cache <<- solve
        getsolve <- function() cache
        list(setcache = setcache, getcache = getcache,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This funtion computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, cachesolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        cache <- x$getsolve()
        if(!is.null(cache)) {
                message("I am getting cached data")
                return(cache)
        }
        data <- x$getcache()
        cache <- solve(data, ...)
        x$setsolve(cache)
        cache
}
