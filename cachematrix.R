## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix create a matrix with that contains following functions
# set: set the value of the matrix
# get: get the value of the matrix
# setinverse: set the inverse value
# getinverse: get the inverse value

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function calculate the inverse of a matrix.  If the inverse value was already calculated
# we skip the calculation and return the calculated value.  If not, the inverse is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
