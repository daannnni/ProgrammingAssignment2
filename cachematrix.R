## These two functions cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special matrix or retrieves
## the cached inverse if it has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse cache as NULL
        
        # Function to set a new matrix 'y' and clear cached inverse
        set <- function(y) {
                x <<- y      # Assign new matrix to 'x' in the parent environment
                inv <<- NULL # Reset inverse cache since matrix has changed
        }
        
        # Function to return the current matrix 'x'
        get <- function() x
        
        # Function to cache the inverse matrix 'inverse'
        setinverse <- function(inverse) inv <<- inverse
        
        # Function to retrieve the cached inverse matrix
        getinverse <- function() inv
        
        # Return a list of the above functions to access them externally
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Try to get cached inverse
        inv <- x$getinverse()
        
        # If cached inverse exists, return it with a message
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        } 
        
        # Otherwise, get the matrix data
        data <- x$get()
        
        # Compute the inverse using solve()
        inv <- solve(data, ...)
        
        # Cache the newly computed inverse for future calls
        x$setinverse(inv)
        
        # Return the inverse matrix
        inv
}
