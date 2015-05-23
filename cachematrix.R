## The functions in this file help create a cache matrix and create it's
## inverse as well as keep it's inverse in a cache using scoping rules of R language


## makeCacheMatrix creates a matrix object with ability to get set it's value
## and it's inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates inverse of a matrix if it doesn't exist or returns 
## the inverse if it exists in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
