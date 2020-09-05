## The functions will cache and return the inverse of a matrix


## This is the first function. It will create a matrix object
## than can cache its inverse
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## The second function calculates the inverse of the matrix created by the fisrt function
## This function will return the inverse of the cache if it was already calculated and 
## the matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmean()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmean(inv)
  inv
}
