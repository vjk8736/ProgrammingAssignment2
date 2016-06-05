## "makeCacheMatrix" creates a special matrix object that can cache its inverse.
## "cacheSolve" computes the inversse of the special matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve should retrieve the inverse fro the cache.


## This function "makeCacheMatrix" prepares a matrix that is appropriate for use
## in "cacheSolve" function.
## Function name     : makeCacheMatrix
## Function argument : matrix object
## Function output   : modified matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function "cacheSolve" computes the inverse of a matrix (assuming the matrix
## is invertible). The matrix inverse is stored in cache for later use.
## Function name     : cacheSolve
## Function argument : modified matrix object from "makeCacheMatrix"
## Function output   : inverse of the input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
