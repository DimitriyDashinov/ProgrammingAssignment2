## The first function, makeMatrix creates a special "matrix", which is really a 
## list containing a function to 1) set the value of the matrix; 2) get the value 
## of the matrix; 3) set the value of the inverse (using the solve function) 
## and 4) get the value of the inverse. 

makeMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setmean function.

cacheinverse <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## How does it work: cacheinverse(makeMatrix(x = matrix( defined your matrix here )))