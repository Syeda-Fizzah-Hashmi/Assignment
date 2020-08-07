# This function intializes the matrix
makeCacheMatrix <- function(x = numeric()) {
        i <- NULL
        # Values assignment to matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # This function returns matrix
        get <- function() x
       
        # Saving calculated inverse of matrix in 'inv'
        setinv <- function(inv) i <<- inv
        
        #Returning value of inverse
        getinv <- function() i
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# Function that accepts x (matrix)
cacheSolve <- function(x, ...) {
        # Calling getinv() 
        i <- x$getinv()
        # Checking if result is already available
        if(!is.null(i)) {
                # Then retrieve that
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        # If not than get new calculated results of inverse
        i <- solve(data, ...)
        x$setinv(i)
        i
}
