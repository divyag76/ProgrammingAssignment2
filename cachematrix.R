## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


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
         setinverse = setinverse,
         getinverse = getinverse)
  }


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x' 
## gets the cached data if it is cached earlier otherwise computes it 
cacheSolve <- function(x, ...) {
  ## Checking for cached data and return message or inverse   
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data") 
    return(inv)
  }
  ## compute the inverse since it is not cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Testing data
# b<-matrix(c(1,2,3,4),2,2)
# b1 <-makeCacheMatrix(b)
# cacheSolve(b1)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
