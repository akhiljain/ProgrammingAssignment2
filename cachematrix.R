## This file contains 2 different functions to cache the inverse of a matrix
## Author : Akhil Jain
## Date Created : 25th September 2017


## This function creates a special "matrix" object that can cache its inverse
## Assumes that the matrix supplied is invertible.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
    ## to the functions with the $ operator
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

## Example usage

# x <- makeCacheMatrix()
# x$set(matrix(c(4,2,7,6), nrow = 2, ncol = 2))
# cacheSolve(x)
#       [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

# cacheSolve(x)
# [1] "getting data from cache"
#       [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting data from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
