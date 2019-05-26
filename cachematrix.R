## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        inv <- NULL
        
        ## Method to set the matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        ## Method the get the matrix
        get <- function() x
        
        ## Method to set the inverse of the matrix
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        
        ## Method to get the inverse of the matrix
        getInverse <- function() inv
        
        ## Return a list of the methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## Just return the inverse if its already set
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using solve function
        inv <- solve(data)
        
        ## Set the inverse to the object
        x$setInverse(inv)
        
        ## Return the matrix
        inv     
}

