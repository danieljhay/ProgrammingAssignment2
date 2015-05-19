## We create two functions, makeCacheMatrix and cacheSolve, for computing and
## caching matrix inverses. By caching the inverse along with the matrix, we
## need only compute the inverse of a given matrix once, and may conserve
## computation time.


## makeCacheMatrix takes a matrix x as input and returns a "cached matrix",
## ie. a list of four functions allowing for the simultaneous caching of a
## matrix and its inverse. If no matrix x is input, by default it will cache
## an empty matrix.
## 
## The four functions---set, get, setInverse, getInverse---establish the
## means by which the cacheSolve function may access and record the actual 
## matrix x underlying the cached matrix, as well as its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve accepts a "cached matrix", ie. a list as created by
## makeCacheMatrix, and returns the inverse of this cached matrix. It also
## caches the inverse of the cached matrix for future usage.
##
## If the inverse of this cached matrix has already been computed (and stored
## via the setInverse() function), cacheSolve will return the cached inverse
## via getInverse(); otherwise, it will compute the inverse from scratch. 

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
