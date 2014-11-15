## These functions eliminate excessive computation when determining the
## inverse of a matrix by cacheing its value for future use.

## This function creates a special "Matrix" object that can potentially
## store and retrieve information on the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(new_matrix){
        x <<- new_matrix
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of a matrix and caches it for 
## further use.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
