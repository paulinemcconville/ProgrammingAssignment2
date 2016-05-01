## There are two functions below
## 1. makeCacheMatrix returns a special type of matrix that can cache (save) its inverse
## four functions are defined within makeCacheMatrix to allow this to happen
## 2. cacheSolve returns the inverse of the matrix returned by makeCacheMatrix 

## nstall the MASS package to allow use of the function 'ginv' that returns the inverse
## of any matrix. I could have used solve but ginv works on non square matrices.
install.packages("MASS")
library(MASS)

## makeCacheMatrix has one parameter, a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set is a function that simply sets the matrix
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  
  ## get is a function that simply returns the matrix
  get <- function() x
  
  ##the function setinv returns the inverse matrix passed to it
  setinv <- function(inverseMatrix) m <<- inverseMatrix
  
  ##the function getinv returns the inverse of the matrix
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
  
## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data)
  x$setinv(m)
  m
}