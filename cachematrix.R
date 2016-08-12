## makeCacheMatrix and cacheSolve takes a matrix and returns the inverse of the matrix if it exists, otherwise
## it returns the same matrix


## makeCacheMatrix function creates a matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set=set, get=get, setinverse=setInverse, getinverse=getInverse)
}


## cacheSolve function will compute the inverse of the input matrix and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Retrieving Cached Data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
