## Functions to deal with setting and caching
## the inverse of large matrices

## sets up a special matrix type to:
## set / get matrix  and set / get inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invrs) inv <<- invrs
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns a matrix that is the inverse of 'x':
## if already computed, then returns the cached value,
## otherwise, computes the inverse and stores in the cache
## (in case of a non-square matrix,  the MASS package
##  is needed to compute the Moore--Penrose inverse)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    ## inverse already in cache, return this value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## else, compute the inverse
    ## (if mtx not square, use the Moore--Penrose generalized inverse)
    mtx <- x$get()
    if(nrow(mtx) == ncol(mtx)) {
        inv <- solve(mtx)
    } else {
        require(MASS)
        inv <- ginv(mtx, ...)
    }
    x$setinverse(inv)
    inv
}
