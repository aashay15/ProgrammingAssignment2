## Put comments here that give an overall description of what your
## functions do

## This function below creates special matrix object that can cache its inverese

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## The below function checks for the cache value of the matrix inverse if it finds the value,
## then it will load from the cache other wise it will run the calculation 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
