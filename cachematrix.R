## This .R document includes two functions: makeCacheMatrix and cacheSolve.
## They are used to calculate the inverse of one matrix
## once got the inverse of the matrix, it will be cached and will not be
## calculated again

## The makeCacheMatrix function is used to create one Object, 
## which has two attributes: the matrix 'x' and its inverse 'inverse'
## and has 4 functions: set, get, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inv) inverse <<- inv
        
        getInverse <- function() inverse
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## give you the matrix x's inverse, which is cached
## if not, calculate the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message('getting cached data')
                return(inverse)
        }
        data <- x$get()
        message('calculating the inverse...')
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse
}
