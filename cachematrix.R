## This function handles making cache of input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMat <- function(y){
        x <<- y
        inv <<- NULL
    }
    getMat <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(setMat = setMat, getMat = getMat,
         setInv = setInv, getInv = getInv)
}


## This function verifies whether the matrix inverse exists in cache, and outputs
# the result, either from cache, or by a new round of calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$getMat()
    inv <- solve(mat,...)
    x$setInv(inv)
    inv
}
