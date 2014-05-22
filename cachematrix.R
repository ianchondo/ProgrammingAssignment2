## makeCacheMatrix creates a special matrix object 
## the function (inverse) calculates the inverse of the matrix
## finds it in the cache and not calculates it again

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix created with
## makeCacheMatrix function (see above)
## If the cached inverse is available cacheSolve retrieves it, 
## If the cached inverse is not available cacheSolve calculates it,
## caches it, and returns it.

cacheSolve <- function(x, ...) {
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
        }
}
