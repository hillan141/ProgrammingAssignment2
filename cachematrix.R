## R programming Assignment 2
## AH, 24 January 2015

## This function prepares an object that
## will be used to cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL
  # The set function sets the matrix and
  # initializes the inverse to NULL
  # in the function's environment
  set <- function(y) {
      x <<- y
      inv.x <<- NULL
  }
  get <- function() x
  setinv <- function(inverse.x) inv.x <<- inverse.x
  getinv <- function() inv.x
  # Return a list containing the getter and
  # setter functions
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## This function returns the matrix inverse.
## It uses the cached inverse if it's been 
## previously computed; otherwise, it will compute
## and cache the inverse.
## This approach is sometimes termed "memoization"

cacheSolve <- function(x, ...) {
    # If the inverse has already been cached
    # return the cached version
    my.inv <- x$getinv()
    if (!is.null(my.inv)) {
        message("using cached inverse")
        return(my.inv)
    }
    # Othwerwise, get the matrix
    # then compute and cache the inverse
    inmat <- x$get()
    # Per instructions, we'll assume matrix is invertible
    my.inv <- solve(inmat, ...)
    x$setinv(my.inv)
    my.inv
}

# Here is an example of how to test the above functions
# Create a 1000x1000 matrix
# mm <- diag(1000)*.025
# xo <- makeCacheMatrix(mm) # prepare the cache object
# system.time(cs <- cacheSolve(xo)) # time the inverse calculation (first time)
# system.time(cs <- cacheSolve(xo)) # time the inverse (from cache); faster
