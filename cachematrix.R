## Coursera - R Programming Module - Week 3 Assignment
## First function will create a special matrix object that can cache its inverse for use later on
## Second function will compute the inverse of the special matrix, unless the inverse has already
## been cached, in which case it will pull the cached inverse

getwd()
setwd("~/GitHub/ProgrammingAssignment2")

## Function 1 creating a special matrix object and caching the inverse of this matrix

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


## Second Function to compute the inverse unless it has already been cached

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

## Testing

g <- matrix(c(-1, 1, 3/2, -1), nrow = 2, ncol = 2)
solve(m)

a <- makeCacheMatrix1(m)

cacheSolve(a)

