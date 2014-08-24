## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a matrix object, x, then creates the methods that will be used by cacheSolve to store the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
##This function returns the inverse of x, if it's already cached, and if not, solves for the inverse and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
