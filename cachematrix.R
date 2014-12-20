##Damien Edwards
## Programming assignment 2
## The cachematrix.R script contains the below functions
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Function that prepares a matrix object to caches its inverse function
## Find the inverse amd return the cache matrix and not calculate it again

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversem<- function(inverse) inv <<-inverse
  getinversem <- function() inv
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem)
}


## Function will run the inverse if the matrix isn't cached
## Returns the inverse matrix if it is cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinversem()
  if (!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  } else {
    inv_x <- solve(x$get())
    x$setinversem(inv)
    return(inv)
  }
}
