## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" which can cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Sets the inverse to NULL
        i <- NULL
        ## Function that allows you to set the value of the matrix
        set <- function(y) {
                ## Sets value of matrix and inverse to NULL
                x <<- y
                i <<- NULL
        }
        ## Function to get value of the matrix
        get <- function() x
        ## Function to set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        ## Function to get the value of the inverse
        getinverse <- function() i
        ## Function that lists the various functions
        list(set = set, get= get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special matrix created from above.
## If the inverse already has been calculated it uses the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## Assumes that the matrix is invertible (i.e. Square)  
        rows <- nrow(x$get())
        d <- diag(rows)
        i <- solve(x$get(), d)
        x$setinverse(i)
        i
}
