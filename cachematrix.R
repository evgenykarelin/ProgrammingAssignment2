## The functions below allow to cache the inverse of matrix.

## makeCacheMatrix() creates an R object that stores a matrix and its inverse.
## It returns a list of functions to the parent environment

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

## cacheSolve() requires an argument that is returned 
## by makeCacheMatrix() in order to retrieve the inversed matrix from the cached  
## value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}