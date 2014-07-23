## Function makeCacheMatrix creates special matrix object that can
## cache its inverse.
## cacheSolve computes the inverse of the special matrix object
## returned by makeCacheMatrix, or retrieves the inverse from cache
## if already calculated.

## makeCacheMatrix - function creates special matrix object that
## caches its own inverse

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


## cacheSolve - function retrieves calculated inverse from cache, else
## computes inverse of makeCacheMatrix if not already in cache

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}




#x<-square invertible matrix
