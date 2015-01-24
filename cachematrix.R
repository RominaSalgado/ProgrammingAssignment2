## makeCacheMatrix creates a special matrix object and cacheSolve 
## calculates the inverse of the matrix.

## The makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## Return a matrix that is the inverse of 'x'. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
       message("getting cached inverse matrix")
       return(inv_x)
       } 
  data <- x$get()
  inv_x <- solve(data,...) 
  x$setinverse(inv_x)
  inv_x
  
}
