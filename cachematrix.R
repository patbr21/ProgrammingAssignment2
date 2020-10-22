## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## define the cache m# get the value of the vector
## assign the input matrix y to the variable x in the parent environment
## re-initialize m in the parent environment to null
## return the matrix x
## set the cache m equal to the inverse of the matrix x
## return the cached inverse of x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               
        set <- function(y) {
                x <<- y         
                m <<- NULL      
        }
        get <- function() x     
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x', or get cahehd data, if you already have a matrix that is the inverse of x

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
