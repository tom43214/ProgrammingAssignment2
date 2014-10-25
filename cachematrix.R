## These functions enable one to create an object for caching a matrix
## and its inverse as well as compute and store the inverse of the 
## matrix.

## Creates a matrix object containing a matrix and its inverse, returning
## a list of functions for setting and getting the matrix and setting and
## getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## Computes the inverse of the matrix stored in a cache matrix object,
## storing the inverse in the object and returning the value of the inverse
## If the inverse has already been set, returns stored inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv   ## Return a matrix that is the inverse of 'x'
}
