##*****************************************************************************
## Programming Assignment 2.
## Write a pair of functions that cache the inverse of a matrix.
## The two functions are:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can
##      cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above. If the inverse has already 
##      been calculated (and the matrix has not changed), then 
##      cacheSolve should retrieve the inverse from the cache.
##*****************************************************************************

##*****************************************************************************
## Here is the function makeCacheMatrix.
## This function creates a special "matrix" object that can cache its inverse.
## It is intended to be used with cacheSolve.
## Input: my.matrix, a matrix to be inverted
## Output: A list of functions:
##      set:    Resets my.matrix and reinitializes its inverse to NULL
##      get:    Returns the value of my.matrix.
##      setInv: Assigns its argument to the variable Inv, which is the cached
##              value of the inverse of my.matrix. In cacheSolve, 
##              the argument of setInv will be the 
##              computed inverse of my.matrix.
##      getInv: Returns the value of Inv, i.e., the cached inverse 
##              (or NULL if the cache is empty)
##*****************************************************************************

makeCacheMatrix <- function(my.matrix = matrix()) {
        Inv <- NULL
        set <- function(some.matrix) {
                my.matrix <<- some.matrix
                Inv <<- NULL
        }
        get <- function() my.matrix
        setInv <- function(inverse) Inv <<- inverse
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}

##*****************************************************************************
## Here is the function cacheSolve.
## This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above. If the inverse has already 
##      been calculated (and the matrix has not changed), then 
##      cacheSolve should retrieve the inverse from the cache.
## Input: my.CacheMatrix, the output of makeCacheMatrix, 
##      which is a list of functions
##      whose environment contains my.matrix, the matrix to be inverted.
## Output: The inverse of my.matrix.
##*****************************************************************************

cacheSolve <- function(my.CacheMatrix, ...) {
        Inv <- my.CacheMatrix$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- my.CacheMatrix$get()
        Inv <- solve(data, ...)
        my.CacheMatrix$setInv(Inv)
        Inv 
}
