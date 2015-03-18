## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatric function handles the caching of value
##    Caching is done in the global environment where the functions are defined
##
## cacheSolve function checks and returns the inverse of the matrix if 
##    it is present in the cache, 
##    else calculates and caches the value before returning it

## Write a short comment describing this function
## makeCacheMatrix function has four methods
##    set the matrix in the cache
##    get the matrix from the cache
##    setmatrix calculates the inverse of the matrix and sets in the cache
##    getmatrix returns the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setmatrix <- function(solve) m <<- solve
    
    getmatrix <- function() m
    
    list(
        set = set,
        get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix
    )
}


## Write a short comment describing this function
## Check if the inverse of the matrix is already available in the cache
## If available, return it
## Else, calculate the inverse and set the cache

cacheSolve <- function(x, ...) {
    message("Checking cache ...")
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("Getting cached data ...")
        return(m)
    }
    
    message("No cached value, computing and caching ...")
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
