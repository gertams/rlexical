
## This function takes a matrix as argument, used to set and get it's value. 
#It must be a square matrix to also set and get the inverse. You can e.g. pass M <- cbind(c(-1,1),c(1.5,-1)) into makeCacheMatrix to test.
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   print(x) #Check to see if Matrix is passed correctly
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) inv <<- solve
   getinverse <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function checks to see if an inverse was calculated. If so, it gets the inverse from cache, if not it calculates it.
## Pass the result of makeCacheMatrix as argument to test this function.
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
   ## Return a matrix that is the inverse of 'x'
}


