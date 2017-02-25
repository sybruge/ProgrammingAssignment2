## This function creates a "cache matrix" which is a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            ## function set : set the value of the matrix
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            ## function get : get the value of the matrix
            get <- function() x
            ## function setinverse : set the value of the inverse matrix
            setinverse <- function(inverse) m <<- inverse
            ## function getinverse : get the value of the inverse matrix
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

## This function calculate the inverse matrix of the "cache matrix" above
## It only calculates if it has not been calculate yet

cacheSolve <- function(x, ...) {
			# get the inverse matrix in the cache
            m <- x$getinverse()
            # If the inverse matrix has been already calculated, then it returns if directly
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
			# Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix in the cache
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
