## Programming Assignment 2: Lexical Scoping

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It takes an input x, which is an inversible matrix
## And returns back list of set, get, setinverse and getinverse object.
## set : sets the value of the matrix
## get : get the value of the matrix
## setInverse : sets the inverse of the matrix
## getInverse : solves for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
