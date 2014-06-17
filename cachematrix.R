
## Matrix inversion being a costly computation, the following 2 functions will 
## allow caching the inverse of a matrix rather than computing it repeatedly.
##
## The first function "makeCacheMatrix" transforms an input matrix, passed as argument, into a special  
## matrix that can store/cache its inverse. This special matrix defines also a list of 4 functions that 
## allow respectively to set the input matrix, to retrieve it, to set the inverse of the input matrix,  
## and to retrieve it.
## The second function "cacheSolve" computes the actual inverse of the input matrix and stores it in an 
## internal variable that acts as a cache. This is done by invoking the inner functions of the special
## matrix created by "makecacheMatrix".

## For example, to compute the inverse of the following matrix:
## mat<- matrix(c(4,3,3,2),2,2)
## Type the following:
## cacheSolve(makeCacheMatrix(mat))


## This function creates a special "matrix" object that can cache its inverse. It also defines a list
## of 4 inner functions (to set and get the matrix whose inverse we want to compute, and to set and get
## the inverse itself).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL       ## initializes the inverse matrix to NULL. Here "inverse" is the name of 
                        ## a local variable that will be used to store the actual inverse of the matrix.
  set <- function(y) {
    x <<- y             ## Assigns the matrix y passed as argument of set() to the  
                        ## variable x defined in the parent environment.
    inverse <<- NULL    ## Sets the inverse matrix in the parent environment to NULL (since
                        ## the matrix has been set to a new value, the existing inverse is no 
                        ## longer good).
  }
  get <- function() x   ## Returns the input matrix.
  setinverse <- function(new_inverse) inverse <<- new_inverse  ## Assigns the inverse matrix to the variable
                                                               ## "inverse" defined in the parent environment.
  getinverse <- function() inverse                    ## Returns the inverse stored in the variable "inverse".
  list(set = set, get = get,                          ## Defines the inner list of the 4 functions.
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()   ## Gets the inverse stored in the cache.
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the value stored in the cache is returned.
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  } 
  
  ## Else, returns a matrix that is the inverse of the matrix 'mat' newly passed as argument.
  mat <- x$get()              ## Gets the input matrix whose inverse we wnat to compute.
  inverse <- solve(mat, ...)  ## The function "solve" does the actual computation of the inverse, 
  x$setinverse(inverse)       ## stores it in the cache "inverse",
  inverse                     ## then returns it.
}

