## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a vector of functions which are then used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y # set x to the value in y, x is in the parent environment 
    n  <<- NULL # set n to Null
  }
  get <- function() x  # this simply gets the matrix x that has been set inside the "set" function
  setmatrix <- function(InvMatrix) 
    n <<- InvMatrix # put the matrix to be inverted in n. That 
  getmatrix<- function() n # cached matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
## Thie function Cache Solve finds inverse of a matrix input and passes it into cache
cacheSolve <- function(x,...) {   ## z is the function makeCacheMatix - need to specify the matrix by passing it as an argument
  m <- x$getmatrix()              ## the matrix to be inverted is obtained using getmatrix function within makeCacheMatrix
  if(!is.null(m)) {
    message("getting cached data") ## get cached data if m is not nukk
    return(m)
  }
  A<- x$get()  # store matrix to be inverted in A
  m <- solve(A, ...)  # calculate the inverse
  #z$getmatrix(m)      # pass the inverse
  m
}
