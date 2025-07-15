## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Return a list of functions that sets a new matrix, 
## get the matrix, set the inverse, and get the inverse

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


## Write a short comment describing this function
## Check if an inverse is already cached. If not, compute the inverse
## store the inverse in the cache, return the computed cache 

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
