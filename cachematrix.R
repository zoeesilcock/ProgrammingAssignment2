## Put comments here that give an overall description of what your
## functions do

## Returns a matrix with extra functionality which allows calculating the
## inverse of the matrix and caches the result.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(slv) s <<- slv
    getSolve <- function() s
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Takes a cached matrix and fetches the cached inversion if one exists or
## otherwise it calculates the inverse and caches the result.

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if (!is.null(s)) {
        message("Getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setSolve(s)
    s
}
