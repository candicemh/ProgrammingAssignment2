## This function creates a special "matrix" object 
## that can cache its inverse.

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


## This function computes the inverse of the special "matrix". 
## If the inverse has already been calculated, 
## the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
      # Attempts to return a matrix inverse 'm' from the cache  
	m <- x$getinverse()
	
	#if m was in the cache, then m wont be null
	if(!is.null(m)) {
      	message("getting cached data")
		return(m)
	}
      
	#get the matrix to invert
	data <- x$get()

	#invert the matrix
	m <- solve(data, ...)
	
	#write the matrix / inverse to the cache
	x$setinverse(m)

	#return the inverse 
	m
}

