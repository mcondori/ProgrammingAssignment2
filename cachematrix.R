## Caching the Inverse of a Matrix

##This function creates a special "matrix" object
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matriz <- NULL
  #Set the value of the matrix
  set <- function(y){
    x <<- y
    matriz <<-NULL
  }
  #Get the value of the matrix
  get <- function() x
  
  #Set the value of the inverse matrix
  setInverse <- function(inverse)
    matriz <<-inverse
  
  #Get the value of the inverse matrix
  getInverse <-function() matriz
  
  list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)

}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix function above
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  matriz <- x$getInverse()
  
  #Ask if the inverse is already calculated, the return it
  if(!is.null(matriz)){
    print("get cache matrix")
    return(matriz)
  }
  
  #The inverse is not yet calculated, then calculate and return it
  matriz <- solve(x$get())
  x$setInverse(matriz)
  matriz
}