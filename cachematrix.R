## Program that takes in a matrix and stores the inverse of the matrix
## to be cached so it can be called quickly

## This function caches the inverse of a matrix passed in as the argument
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


## This function calls the cached inverse matrix
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
