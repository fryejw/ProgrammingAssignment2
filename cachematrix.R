## R Programming - Programming Assignment 2
## Using Lexigraphical Scoping to store inverted matricies
##      to reduce computation time if inverting the same matrix
##      occurs often in the program.

 

## This function is the caching function of the matrix inverse.
## We set the matrix to itself and reset the inverse to NULL.
## We then computer the inverse of the matrix and wrap it together
## with the original matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse.x <- NULL
    set <- function(y){
        x <<- y
        inverse.x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse.x <<- solve
    getinverse <- function() inverse.x
    list(set = set
         ,get = get
         ,setinverse = setinverse
         ,getinverse = getinverse)
}


## Take a wrapped matrix that's returned from the makeCacheMatrix
## function and output the inverse of the matrix. This will be quick
## if it's repeatedly called on the same matrix since we're caching the
## inverse through the use of the scoping assignment <<- in the previous
## function.

## If it is a new matrix, we reset the cached values and lose the caching
## of the original matrix inverse to gain the caching of the new matrix
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse.x <- x$getinverse()
    if(!is.null(inverse.x)) {
        message("getting cached data")
        return(inverse.x)
    }
    data <- x$get()
    inverse.x <- solve(data, ...)
    x$setinverse(inverse.x)
    inverse.x
}

