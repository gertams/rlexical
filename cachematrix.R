## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as argument, used to set and get it's value. 
#It must be a square matrix to also set and get the inverse
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   print(x) #Check to see if Matrix is passed correctly
   set <- function(y) {
      x <<- y
      print(y) #weg
      inv <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) inv <<- solve
   getinverse <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function checks to see if an inverse was calculated. If so, it gets the inverse from cache and else calculates it
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   #print(x) #weg
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



# make.power <- function(n){
#    pow <- function(x){
#       x^n
#    }
#    pow
# }
