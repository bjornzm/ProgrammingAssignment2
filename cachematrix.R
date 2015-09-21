## Computes and caches the inverse of a square matrix
## Example:
##   m = makeCacheMatrix(matrix(1:4, 2, 2))
##   cacheSolve(m)
##   cacheSolve(m)
## The second invocatino will return the cached result


## Returns a list of functions to cache the computed values

makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL
    set <- function(y) {
        x <<- y
        cached <<- NULL
    }
    get <- function() x
    setcached <- function(computed)  cached <<- computed
    getcached <- function() cached
    list(set = set, get = get,
         setcached = setcached,
         getcached = getcached)
}


## Inverts the square matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    cached <- x$getcached()
    if (!is.null(cached)) {
        message("getting cached data")
        return(cached)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setcached(inverse)
    inverse
}
