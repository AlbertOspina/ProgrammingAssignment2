## These pair of functions will cache a inverse of a matrix to avoid of
## calculating it every time you needed.

## Creates a list containing the functions set, get, setinv and getinv receiving
## a matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Gets the cached inverse of a matrix if it was calculated before, otherwise
## computes it and returns it. Receives the list returned from a makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix (i) that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}