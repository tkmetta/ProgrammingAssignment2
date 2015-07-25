## Put comments here that give an overall description of what your
## functions do

## Sample usage
## > source("cachematrix.R")
## > mm <-matrix(c(2,8,29,1),nrow=2,ncol=2)
## > mat <-makeCacheMatrix(mm)
## > b <-cacheSolve(mat)
## check what's in b
## > b
## [,1]         [,2]
## [1,] -0.004347826  0.126086957
## [2,]  0.034782609 -0.008695652
## call again..get from cache
## > c <-cacheSolve(mat)
## getting cached data


## Write a short comment describing this function
## creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(minverse) m <<- minverse
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function
## Check if inverse already calculated
## if so get's from cache & skips computation
## if not calculate and sets in cache

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
