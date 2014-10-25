## The below two functions are built to create a special object 
## that stores a matrix and caches its inverse matrix.


## The function makeCacheMatrix creates a list containing a function 
## to set and get the values of a matrix and set and get the values 
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() {
                x
        }
        setinv <- function(solve) {
                inv <<- solve
        }
        getinv <- function() {
                inv
        }
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function checks if there has already been calculated 
## the inverse matrix of the matrix defined in makeCacheMatrix. If so, it
## gets the inverse from the cache and skips calculation of the inverse, 
## otherwise it calculates it and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}