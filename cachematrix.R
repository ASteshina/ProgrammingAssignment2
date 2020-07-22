## makeCacheMatrix is a funtion that creates a matrix object 
## and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # Initially setting the inverse to null
        inverse <- NULL
        
        # Set the value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # Get the value of the matrix
        get <- function() {
                x
        }
        
        # Set the value of the inverse
        cacheInverse <- function(solve){ 
                inverse <<- solve
        }
        
        # Get the value of the inverse
        getInverse <- function() {
                inverse
        } 
        
        # return: a list containing functions to
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse
        # 4. get the inverse
        # This list is used as the input to cacheSolve()
        list(set = set, get = get,
             cacheInverse = cacheInverse,
             getInverse = getInverse) 
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of 'x'
        inverse <- y$getInverse()
        
        # If the inverse has already been calculated
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # Otherwise, calculate the inverse
        data <- y$get()
        inverse <- solve(data,...)
        y$cacheInverse(inverse)
        
        inverse
}
