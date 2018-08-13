## Function designed to calculate and cache the inverse of a Matrix
## Utilizing cacheSolve will check to see if it's already solved and 
## retrieve cached value instead of recomputing

## Creates a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This checks for existing solve and retrieves cached value if already solve
## If not, runs the function to solve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
    }
