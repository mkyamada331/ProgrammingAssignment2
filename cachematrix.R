## makeCacheMatrix and cacheSolve takes a matrix and returns the inverse of the matrix if it exists, otherwise
## it returns the same matrix


## makeCacheMatrix function creates a matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve function will compute the inverse of the input matrix and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Retrieving Cached Data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        return (inv)
}
