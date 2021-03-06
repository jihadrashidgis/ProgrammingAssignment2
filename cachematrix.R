## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function will take the object (list of functions) returned by makeCacheMatrix(). 

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
##This function computes the inverse matrix return by makeCacheMatrix function &
##retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  j <- x$getinverse()
  if(!is.null(j)) {
    message("Getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
        ## Return a matrix that is the inverse of 'x'
}
