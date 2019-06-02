## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        MyCachedMatrix <- NULL
        set <- function(y) {
                x <<- y
                MyCachedMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) MyCachedMatrix <<- inverse
        getinverse <- function() MyCachedMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function finds the inverse matrix of a given square matrix, returned by makeCacheMatrix funciton above.
## If the inverse matrix has already been calculated, and the given  matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        MyCachedMatrix <- x$getinverse()
        if(!is.null(MyCachedMatrix)) {
                print("Getting cached inverse matrix")
                return(MyCachedMatrix)
        }
        data <- x$get()
        MyCachedMatrix <- solve(data, ...)
        x$setinverse(MyCachedMatrix)
        MyCachedMatrix        
}
