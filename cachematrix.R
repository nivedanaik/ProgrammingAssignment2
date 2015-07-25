## The first function "makeCacheMatrix" is creating a matrix object that is used to cache the inverse of the matrix.
## The second function "cacheSolve" is calculating the inverse of the matrix passed by the first function.
## If the inverse has already been calculated  then cacheSolve function will return the inverse of the matrix.

##this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinv <- function (inv) m <<- inv
  getinv <- function () m
  
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Return the inverse if already calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <-x$getinv()
  if(!is.null(m)){
    message(" getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <-solve(data, ...)
  x$setinv(m)
  m
  
}