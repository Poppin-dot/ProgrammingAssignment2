## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse property
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset the inverse property when the matrix is changed
                }
  
        get <- function() x  # Function to get the value of the matrix
  
        setInverse <- function(inverse) inv <<- inverse  # Function to set the value of the inverse
  
        getInverse <- function() inv  # Function to get the value of the inverse
  
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()  # Retrieve the inverse from the cache
  
        if(!is.null(inv)) {  # Check if the inverse is already cached
                message("getting cached data")
                return(inv)  # Return the cached inverse
  }
  
        data <- x$get()  # Get the value of the matrix
        inv <- solve(data, ...)  # Compute the inverse of the matrix
        x$setInverse(inv)  # Cache the computed inverse
        inv
        ## Return a matrix that is the inverse of 'x'
}
