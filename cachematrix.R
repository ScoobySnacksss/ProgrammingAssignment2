## This pair of functions calculates the inverse of a square
## invertible matrix or returns the previously calculated 
## inverse matrix by using the double arrow assignment operator
## that ensures that the parent environment (outside the closure
## of the function) is assigned the same 'inv' value as is assigned
## inside the low level 'makeCacheMatrix' function.

## Function 'makeCacheMatrix' creates 4 functions that apply to an
## an input matrix argument: 'set', 'get', 'setinv', and 'getinv'. When
## the Inverse is either calculated ('setinv is' called) or nullified
## ('set' is called) the "value" of inv (or, inverse of input matrix)
## is "seen" across the makeCacheMatrix function so that the crucial
## 'getinv' function has the most up to date version of the inverse matrix
## stored in cache. Other "parent" functions that call the 'getinv' function
## (like function 'cacheSolve' defined below) will be able to simply 
## retrieve a previously calculated inverse matrix or use the null value
## as a hint to proceed to calculate the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function 'cacheSolve' either finds the inverse of an
## square invertible matrix if it's the first time
## the function is called. Otherwise, function 'cacheSolve'
## simply retrieves the previously calculated inverse matrix
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
  
	data <- x$get()
  
	inv <- solve(data, ...)
  
	x$setinv(inv)
  
	inv	
}
