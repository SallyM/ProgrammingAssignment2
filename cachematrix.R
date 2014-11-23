## These functions cache the inverse of a matrix

## creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## setting the matrix
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        
        ## getting the matrix
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmean(m)
        m
}
