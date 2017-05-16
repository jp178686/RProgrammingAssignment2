## The purpose of this exercise is to create a pair of functions that
## cache the inverse of the matrix. The matrix is a square matrix (invertible).

## This first function (makeCacheMatrix) will create a speacial "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        slv <- NULL
        set <- function(y) {
                x <<- y
                slv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) slv <<- solve
        getinv <- function() slv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function, "cacheSolve", will caculate the inverse of the matrix set
## above. If the inverse has already been calculated and the matrix remains
## unchanged, it will return the cached inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'     
        slv <- x$getinv()
        if (!is.null(slv)) {
                message("getting cached data")
                return(slv)
        }
        mtx <- x$get()
        slv <- solve(mtx, ...)
        x#setinv(slv)
        slv
        
}
