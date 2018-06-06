## This program includes the functions makeCacheMatric and cacheSolve
## makeCacheMatrix takes a matrix as input, obtaisn the inverse and saves
## all 

## makeCacheMatrix computes the matrix inverse and returns a list
## with the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse to NULL
  MyInv <- NULL
  # set is saved outside of the environment of this function
  set <- function(y) {
    x <<- y
    MyInv <<- NULL
  }
  # get is the original input
  get <- function() x

  setInverse <- function(inverse) MyInv <<- inverse #calculate the inverse
  getInverse <- function() MyInv
  
  #This function returns a list of all of these pieces
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve looks to see if there is an inverse already for the matrix
## and returns that matrix.  The second time it is called it gets the
## cached inverse and is faster than the first

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    MyInv <- x$getInverse()
    if (!is.null(MyInv)) {
     message("getting cached data")
     return(MyInv)
   }
   getthis <- x$get()
    MyInv <- solve(getthis, ...)
    x$setInverse(MyInv)
   MyInv
}

## EXAMPLE CALL
M <- matrix(c(1,4,5,7),2,2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1)
cacheSolve(M1)

