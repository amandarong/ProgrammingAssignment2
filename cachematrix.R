## Assignment 2: write a pair of functions that cache the inverse of a matrix
## Author: Amanda Rong
## Last Modified: Jun 22, 2014

## makeCacheMatrix: 
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse
        setinv <- function(i) inv <<- i
        
        ## get the value of the inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: 
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
