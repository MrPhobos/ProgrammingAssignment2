## If the function 'set' is called, the function will
## will cache (matrix) y in x. With get, it will return simply return the matrix x.
## Setinv will cache 'inverse' to the argument y (not the be used seperately, only)
## by function cacheResolve to cache the real inverse).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL            # set the inverse to NULL
  set <- function(y){
    x <<- y                  # cache y to x
    inverse <<- NULL         # if the matrix is new, it will need a new inverse
  }
  get <- function() x        # simply return x
  setinv <- function(y) inverse <<- y   # cache y to the variable 'inverse'
  getinv <- function() inverse          # simply return the cached inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)   # 'define' the function names
}


## This function checks if the inverse has been calculated and cached of the 'special' matrix 
## crreated by the function makeCacheMatrix. If not, it calculates the inverse and returns this.
## if it already knows the inverse of the matrix, it will return the cached inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()      # Check if there is a chached inverse
  if(!is.null(inverse)) {    # there is... 
    message("Getting cache data... (what an original message!)")
    return(inverse)          # ... return this cached inverse! And we're done.
  }
  data <- x$get()            # ... otherwise, get the matrix x...
  inverse <- solve(data, ...)# ... calculate the inverse ....
  x$setinv(inverse)          # ... and cache this inverse through the setinv function
  inverse                    # and of course, show the result.
}
