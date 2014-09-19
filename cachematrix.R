## The makeCacheMatrix function creates a special "matrix" (e.i., a list of functions) 
## when used together with the cacheSolve function (see COMMON USAGE and OUTPUT EXAMPLE below)
## to cache potentially time-consuming computations. In particular, these functions cache 
## inverse of a matrix.
## 
## NOTE: It is assumed that the matrix supplied to these functions is always invertible
##
## COMMON USAGE
## =============================
## source("cachematrix.R")
## m <- makeCacheMatrix(matrix)
## i <- cacheSolve(m)
## i
##
## OUPUT EXAMPLE
## =============================
## m <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## The makeVector creates a special "matrix" (e.i., a list of functions) that 
##
##    sets the value of the matrix (set() function)
##    gets the value of the matrix (get() function)
##    sets the inverse of the matrix (setinverse() function)
##    gets the inverse of the matrix (getinverse() function)
##
## PARAMETERS AND VARIABLES
##
##  x - is the matrix to inverse
##  i - is the cached value for the inversed matrix (default value is null)


makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The cacheSolve function calculates the inverse of a matrix of the special "matrix" 
## created with the makeCacheMatrix function.
##
## NOTE: It checks to see wether the inverse of the matrix was calculated. 
##       If so, it returns the cached value and do not calculate the inverse of the matrix.
##       If not, it calculates the inverse of the matrix and puts the result in the cache 
##       using the setinverse function.
## 
## PARAMETERS AND VARIABLES
##
##  x - is the special "matrix" created using the to makeCacheMatrix
##  i - is the inversed matrix returned to the user
## data - is the matrix to inverse

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
