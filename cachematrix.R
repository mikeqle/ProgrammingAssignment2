## Get inverse of matrix; pull cached inverse whenever possible;

## Return a list of 4 functions: set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # i stands for inverse
    
    ## Set and get matrix
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    get <- function () x
    
    ## Set and get inverse
    setinverse <- function(inversematrix) i <<- inversematrix
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is an inverse of x
## If there is a cached inverse of x, then pull cached inverse instead
## If there isn't, cache the inverse matrix back into x
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data!")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}