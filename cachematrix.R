
## The first function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y            ## set the value of the matrix in a different environment
        m <<- NULL
    }
    get <- function() x   ## get the value of the matrix from the different environment
    setinverse <- function(inverse) m <<-inverse   ## set the value of the inverse from another environment 
    getinverse <- function() m    ##get the value of the inverse matrix from another environment          
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## the second function calculates the inverse matrix of the matrix created in the first function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()   ##get the cached inverse matrix and store in m
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
