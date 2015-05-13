## Structures for caching the inverse of a the matrix

## Function that creates a matrix with access methods and
## a cache for its inverse.
## The cache is cleaned when the matrix is modified, thus
## invalidating the inverse.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL    # inverse
    set <- function(newMatrixData){
        x <<- newMatrixData
        xinv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xinv <<- inverse
    getInverse <- function() xinv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that returns the inverse of a matrix with cache
## If the inverse had not been calculated it is calculated,
## otherwise the cached value is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getInverse()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    m <- x$get()
    if(nrow(m) != ncol(m)) stop("Non symetric matrix rows=", nrow(m), "cols=", ncol(m))
    inv <- solve(m)
    x$setInverse(inv)
    inv
}

## Test with
# m <- makeCacheMatrix(matrix(rnorm(200),10,10))
# minv <- cacheSolve(m)
# m$get() %*% minv
## or
# m <- makeCacheMatrix(diag(5))  # This one is exact
