## This function creates a "cache matrix" which is a list of 4 functions
## set : set the value of the matrix
## get : get the value of the matrix
## setsolve : set the value of the inverse matrix of the matrix
## getsolve : get the value of the inverse matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) m <<- solve(x)
            getsolve <- function() m
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}

## This function calculate the inverse matrix of the "cache matrix" above
## If the inverse matrix has been already calculated, then it returns if directly
## Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix
## in the "cache"
cacheSolve <- function(x, ...) {
            m <- x$getsolve()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
}
