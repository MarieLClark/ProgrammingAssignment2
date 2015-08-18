## Below are two functions that (1) create a special object that creates a matrix and (2) cache's its inverse.

## makeCasheMatrix creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
