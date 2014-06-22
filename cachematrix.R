## Caching the Inverse of a Matrix

##This function creates a special "matrix" object
##that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  matriz <- NULL
  set <- function(y){
    x <<- y
    matriz <<-NULL
  }
  
  get <- function() x
  setInverse <- function(inverse)
    matriz <<-inverse
  getInverse <-function() matriz
  list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)

}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix function above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matriz <- x$getInverse()
  if(!is.null(matriz)){
    print("get cache matrix")
    return(matriz)
  }
  matriz <- solve(x$get())
  x$setInverse(matriz)
  matriz
}
