## Functions in this file will store a static copy of a matrix
## and its corresponding inverse. These two functions work together
## to allow the reuse of the matrix and its inverse without having 
## to recalculate the inverse multiple times

## makeCacheMatrix
## Parameter: x         a matrix to be used as input
## Functions: 
##      set  (y)        input matrix to be stored by the cache. Will delete the 
##                      cached inverse until it is re-set
##      get             will return the cache matrix
##      setInverse(inverseatrix)        will set the cached inverse matrix
##      getInverse      will return the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x<<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) invMatrix<<-inverseMatrix
        getInverse <- function() invMatrix
        list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
        
}
## end of makeCacheMatrix

## cacheSolve
## function that will make use of a defined makeCacheMatrix in order to store
## return, calculate and store a matrix and its inverse matrix
## Parameter:   x       a store function makeCacheMatrix
## Return: the inverse matrix of the matrix given

cacheSolve <- function(x, ...) {
        ## Return a matrix that bis the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
