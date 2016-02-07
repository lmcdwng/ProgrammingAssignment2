## @x: Assume x is a matrix which be inverted
## @return: a list of functions which set the matrix, get the matrix,
##          set the inverse of the matrix, get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Return the inverse of a matrix 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}