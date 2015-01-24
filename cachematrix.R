## Put comments here that give an overall description of what your
## functions do

## This function prepares a memoization object
## for a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL
  set <- function(y) {
      x <<- y
      inv.x <<- NULL
  }
  get <- function() x
  setinv <- function(inverse.x) inv.x <<- inverse.x
  getinv <- function() inv.x
  list(set = set, get = get, 
       setmean = setmean,
       getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
