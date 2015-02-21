# makeCacheMatrix creates a list which contains a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinver <- function(inver) inv <<- inver
        getinver <- function() inv
        list(set = set, get = get, setinver = setinver, getinver = getinver)

}


# The following function cacheSolve returns the inverse of the matrix. It first checks 
# if the inverse has already been computed. If so, it gets the result and 
# skips the computation. If not, it computes the inverse, sets the value in 
# the cache via setinverse function. This function assumes that the matrix 
# is always invertible.


cacheSolve <- function(x, ...) {

        inv <- x$getinver()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinver(inv)
        inv

}