## Assignment 2: Caching the Inverse of a Matrix
## 
## Writing a pair of functions that cahce the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMat <<- inverse
  getinverse <- function() invMat
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  invMat <- x$getinverse()
  if(!is.null(invMat)) {
    message("getting cached data.")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data,...)
  x$setinverse(invMat)
  invMat
}
