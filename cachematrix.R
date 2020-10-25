## A function that makes a special "matrix" object that can cache its inverse.
## Then a function that figures out the inverse of the "matrix" returned by the above.
## Yet it if has already computed this (and has not changed), it should instead retrieve it from
## the cache.

## Part one: Creating a special "matrix" object that can cache its inverse.
## OBS assumed it's invertible. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL ##assigning a value to a an object in an enviroment different from current) 
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function () m
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Part 2: Return a matrix that is the inverse of x taking care to notice whether it was computed before.

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("Getting cached data.")
                return(m)
        }
        else
                data <- x$get() ##get data of matrix
                m <- solve(data, ...) ## compute
                x$setinverse(m) ## store this in inverse
                m ## also print it
        
}