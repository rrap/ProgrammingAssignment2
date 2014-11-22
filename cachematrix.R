## A pair of functions that cache the inverse of a matrix to avoid costly computations
## 

## makeCacheMatrix - Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setInv <- function(inv) invX <<- inv
    getInv <- function() invX
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve - computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invX <- x$getInv()
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    data <- x$get()
    invX <- solve(data, ...)
    x$setInv(invX)
    invX
}
