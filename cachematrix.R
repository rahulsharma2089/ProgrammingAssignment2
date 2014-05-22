## This script contains two functions that are responsible for computing and
## caching the inverse of the matrix. These functions assume that matrix provided
## is inversible.

##makeCacheMatrix is a function used to create a list containing functions to: 
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      mat.inv <- NULL
      set <- function (y){
            x <<- as.matrix(y)
            mat.inv <<- NULL
      }
      get <- function () x
      setInverse <- function (inv) mat.inv <<- inv
      getInverse <- function() mat.inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve function calculates value of inverse of the matrix if it hasn't
##been already calculated.
##while calculating the inverse function checks whether the matrix is square or
##non-square and proceeds accordingly.
##for non-quare matrix Moore-Penrose generalized inverse is calculated.
cacheSolve <- function(x, ...) {
      mat.inv <- x$getInverse()
      if(!is.null(mat.inv)){
            message("getting cached inverse")
            return(mat.inv)
      }
      data <- x$get()
      data.dim <- dim(data)
      if(data.dim[1] == data.dim[2]){
            mat.inv <- solve(data)
      }
      else{
            library(MASS)
            mat.inv <- ginv(data)
      }
      x$setInverse(mat.inv)
      mat.inv ##return the inversed matrix
}
