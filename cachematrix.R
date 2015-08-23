## akvasanth jmjplsubmission
## Generating matrix inverse using cache
## Sample output shown below the functions

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
    	set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cachesolve function

cacheSolve <- function(x, ...) {
 	inv <- x$getinverse()
    	if(!is.null(inv)) {
        message("Matrix hasn't changed, retrieving cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


##> source("cachematrix.R")
##> x <- matrix(c(1,2,3,4),2,2)
##> x
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
 

##> c <- makeCacheMatrix(x)
##> c$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##> cacheSolve(c)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##> cacheSolve(c)
##Matrix hasn't changed, retrieving cached data...
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

