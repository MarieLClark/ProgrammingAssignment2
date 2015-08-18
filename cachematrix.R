## Put comments here that give an overall description of what your
## functions do

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


## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If it exists and has not changed, it gets the inverse from the 
## cache and skips the computation. If not, it calculates the inverse and sets the rexzults in the cache 
## using the setinv function.

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
