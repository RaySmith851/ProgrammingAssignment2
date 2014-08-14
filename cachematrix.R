## cachematrix.R. Functions to handle storage of a matrix with cached inverse.
# The inverse is computed when first requested and then cached. Subsequent 
# requests simply return the cached value. This makes it more efficient
# than a simple matrix in the event that the inverse is required more than once.
# The functions make use of R's lexical scoping rules to store state data in
# the declaring environment.

## makeCacheMatrix(x = matrix()) constructs a cached matrix with x as the
## stored matrix.

# The returned value, c, is a list of named functions for operating on it:
#   c$set(y) changes the stored matrix to y
#   c$get() returns the stored matrix
#   c$setinv(inv) sets the stored inverse to inv (only used internally)
#   c$getinv() returns the stored inverse matrix (only used internally; use 
#     cacheSolve() instead)

makeCacheMatrix <- function(x = matrix()) {
    # Create a cached matrix with x as the get() value.
    inv <- NULL # inverse is NULL until computed by cacheSolve()
    set <- function(y) {
        x <<- y
        inv <<- NULL # invalidate the inverse as we don't know the new value
    }
    get <- function() x
    setinv <- function(inv_par) inv <<- inv_par
    getinv <- function() inv
    list(set = set, # return the cached matrix as a list of function closures
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve(c) returns the inverse of the matrix stored in c, where c must 
## have been previously returned from a call to makeCacheMatrix().
# On the first call, the inverse is computed and cached. Subsequent calls will
# just return the cached value.

cacheSolve <- function(c, ...) {
    ## Return a matrix that is the inverse of 'c$get()'
    inv <- c$getinv() # get the currently stored value
    if(is.null(inv)) { # if NULL, compute it
        mat <- c$get()
        inv <- solve(mat, ...) # Use R's 'solve' function
        c$setinv(inv) # cache the computed inverse
    }
    inv # return the inverse
}
