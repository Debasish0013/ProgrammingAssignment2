# This function Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        d <- NULL # Define function
        set <- function(y) {
                x <<- y # Value setting
                d <<- NULL # Cache Clearing
        }
        get <- function() x
        setInverse <- function(inverse) d <<- inverse # Define function to get the inverse
        getInverse <- function() d # List with four functions returns
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# This function computes the inverse of the special "matrix"

cacheSolve <- function(x) {
        d <- x$getInverse() 
        if(!is.null(d)) { 
                message("getting cached data")
                return(d)
        } 
        data <- x$get() # Getting the value of matrix
        d <- solve(data) # Inverse calculation
        x$setInverse(d) 
        d # Inversed matrix returns
}
