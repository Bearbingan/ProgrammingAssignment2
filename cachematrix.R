## The basic concepts and procedures are the same as those in the examples. 
## We need two functions to realize our goals.

## The makeCachematix function returns a list consists of functions designed for
## getting or setting the input matrix or the inverse one.  

makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse,getinverse = getinverse)
}


## The cacheSolve function will return the inverse matrix with or without 
## evaluation. If the inverse matrix is already chached, time is saved.
## Time is money!! :)

cacheSolve <- function(x,...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}