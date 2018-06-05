## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  MyInv <- NULL
  set <- function(y) {
    x <<- y
    MyInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) MyInv <<- inverse #calculate the inverse
  getInverse <- function() MyInv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  MyInv <- x$getInverse()
  if (!is.null(MyInv)) {
    message("getting cached data")
    return(MyInv)
  }
  mat <- x$get()
  MyInv <- solve(mat, ...)
  x$setInverse(MyInv)
  MyInv
  
}

