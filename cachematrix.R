# Cache the inverse of a matrix. If the matrix doesn't change, its inverse can be retrieved
# from cache. No need to calculate its inverse again.

# Creates a special 'matrix' object, which is really a list of 4 functions
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and 
# sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
