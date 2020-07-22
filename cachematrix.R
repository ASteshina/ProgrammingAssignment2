## makeCacheMatrix is a funtion that creates a matrix object 
## and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        cacheInverse <- function(solve){ 
                inverse <<- solve
        }
        getInverse <- function() {
                inverse
        } 
        list(set = set, get = get,
             cacheInverse = cacheInverse,
             getInverse = getInverse) 
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- y$getInverse()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- y$get()
        inverse <- solve(data,...)
        y$cacheInverse(inverse)
        
        inverse
}
