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




NCols=1000
NRows=1000
myMat<-matrix(runif(NCols*NRows), ncol=NCols) 


matriz<-makeCacheMatrix(myMat)
system.time(solve(myMat))
system.time(cacheSolve(matriz))
system.time(cacheSolve(matriz))


