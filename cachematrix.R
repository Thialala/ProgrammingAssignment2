## makeCacheMatrix creates a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the inverse of the matrix
## - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv_x <<- NULL                
        }
        
        getMatrix <- function() x
        setMatrixInverse <- function(inv) inv_x <<- inv
        getMatrixInverse <- function() inv_x
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setMatrixInverse = setMatrixInverse, 
             getMatrixInverse = getMatrixInverse)
}


## cacheSolve calculates the inverse of the matrix using the list of functions 
## created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setMatrixInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getMatrixInverse()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$getMatrix()
        inv_x <- solve(data, ...)
        x$setMatrixInverse(inv_x)
        inv_x
}

## A <- matrix(c(2,4,3,5), 2, 2)
## invCalculator <- makeCacheMatrix(A)
## cacheSolve(invCalculator)
