## two functions to caching the inverse of a matrix


## makeCacheMatrix creates a special list to store (set) and get the inverse of a matrix

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


## cacheSolve checks whether the inverse has already been calculated and stored, 
## if so, it gets the inverse from the cache and skips the computation;
## otherwise, it calculates the inverse of the matrix and store in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
