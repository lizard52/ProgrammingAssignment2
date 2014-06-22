## These functions cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(myinv) inv <<- myinv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)){
    message('getting cached inverse matrix')
    return(inverse)
    
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}
