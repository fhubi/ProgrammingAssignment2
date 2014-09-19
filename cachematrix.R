## Pair of functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    i <- x$getinverse()
    if(!is.null(i)) 
    {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
