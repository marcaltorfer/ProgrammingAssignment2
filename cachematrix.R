## With the two functions below I can work on matrices, calculating the inverse of a matrix or
## storing a fixed value for the inverse.

## makeCacheMatrix is a function in which I create and store 4 functions:
## get, set, set inverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
        get <- function()x
        setinverse <- function(inverse)inv <<- inverse
        getinverse <- function()inv
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve is a function that allows me to retrieve the inverse, if it was allready
## stored in makeCacheMatrix or calculate it with the Solve() function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  table <- x$get()
  inv <- solve(table, ...)
  x$setinverse(inv)
  inv
}
## Return a matrix that is the inverse of 'x'
