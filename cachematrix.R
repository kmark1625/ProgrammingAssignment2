## makeCacheMatrix will create a special "matrix" that can cache its own inverse.
## cacheSolve will take a special "matrix" as input and calculate the inverse (using the cache if available.)

## This function creates a special "matrix" which can cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Sets the inverse to NULL
        i <- NULL
        ## Function that allows you to set the value of the matrix
        set <- function(y) {
                ## Sets value of matrix to the given value and inverse to NULL
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


## This function calculates the inverse of the special matrix created from the makeCachematrix function.
## If the inverse already has been calculated, the cached value is used.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Sets the inverse to the current value retreived from the "special" matrix.
        i <- x$getinverse()
        ## If the value is present, the cached value will be returned
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Assumes that the matrix is invertible (i.e. Square) and finds size based on rows 
        rows <- nrow(x$get())
        ## Creates an identity matrix of the appropriate dimensions.
        d <- diag(rows)
        ## solves the inverse and sets the value of the inverse.
        i <- solve(x$get(), d)
        x$setinverse(i)
        i
}
