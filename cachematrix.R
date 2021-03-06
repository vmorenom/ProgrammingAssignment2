## Those functions handle a custom matrix type which is
## capable of caching a previously calculated inverse and
## re-using its value for improving performance if called
## several times


## Create function for creating our custom-type of matrix
## Methods:
##    - set/get: modify/obtain the stored matrix. Setting a new matrix erases the cached inverse value
##    - setinv/getinv: modify/obtain the stored inverse matrix.
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Obtain the inverse matrix of one of our custo matrix.
## If the inverse has already ben set for that matrix, no calculations are done.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}
