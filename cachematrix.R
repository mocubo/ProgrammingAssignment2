# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x) {
        # Initialize variables to store the matrix and its cached inverse
        data <- x
        cached_inverse <- NULL
        
        # Function to set the cached inverse
        setInverse <- function(inverse) {
                cached_inverse <<- inverse  # Use <<- to assign to the parent environment
        }
        
        # Function to get the cached inverse
        getInverse <- function() {
                cached_inverse
        }
        
        # Return a list containing the matrix and the functions
        list(
                data = data,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

# Function to compute the inverse of the special matrix (with caching)
cacheSolve <- function(mat_cache) {
        # Get the matrix from the cache
        x <- mat_cache$data
        
        # Check if the inverse is already cached
        cached_inverse <- mat_cache$getInverse()
        if (!is.null(cached_inverse)) {
                message("Getting cached inverse")
                return(cached_inverse)
        }
        
        # If the inverse is not cached, compute it and cache it
        inverse <- solve(x)
        mat_cache$setInverse(inverse)
        message("Computing and caching the inverse")
        
        # Return the computed inverse
        inverse
}
