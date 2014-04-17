## The two functions below are used to create a special object that stores
## a numeric matrix and caches its inverse; in order to avoid multiple
## computations of the inverse


## This function creates a special "matrix", which is a list
## containing 4 functions:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

		inverse <- NULL

        set <- function(y) {
        	
        		x <<- y
               	inverse <<- NULL
        }
            
        get <- function() x
            
        setinverse <- function(z) inverse <<- z
            
        getinverse <- function() inverse
            
        list(set = set, get = get,
        		setinverse = setinverse,
                getinverse = getinverse)

}


## Write a short comment describing this function
## This function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been cached. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and caches this value via the "setinverse" function

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
            
        if(!is.null(inverse)) {
        		
        		message("getting cached data")
        		
        		return(inverse)
            
        }
            
        data <- x$get()
            
        size <- nrow(data)
        	
      	inverse <- solve(data,diag(1,size,size))
            
        x$setinverse(inverse)
            
        inverse

}