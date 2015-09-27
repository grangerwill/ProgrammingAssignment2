## makeCacheMatrix creates a special "matrix" object that can cache its inverse. It lets you:
        ## set a matrix,
        ## view it (using $get), 
        ## set its inverse (using $setinverse), or
        ## view its inverse (using $getinverse).
## cacheSolve will:
        ## compute the inverse of the special "matrix" returned by makeCacheMatrix above, or
        ## call the inverse from the cache.

## --makeCacheMatrix--
## The syntax for creating an object using this function is as follows:
## [object name] <- makeCacheMatrix(matrix([normal matrix arguments go here]))

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


## --cacheSolve--
## The inverse of the matrix defined in makeCacheMatrix will be either calculated or called from the cache using this function.

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