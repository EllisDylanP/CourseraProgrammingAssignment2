## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function should return the inverse of a given matrix. This function is the list of functions to set the matrix, 
## get the matrix, set the inverse of the matrix, and get the inverse of the matrix

## set the "variable" to identify the matrix to the function. ex: "xx" <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setx <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinvx <- function(solve) inv <<- solve
  getinvx <- function() inv
  list(setx = setx, getmatrix = getmatrix, setinvx = setinvx, getinvx = getinvx)
}


## Write a short comment describing this function
## if the inverse of a matrix is not within the cache then this function should return the inverse of the matrix.
## the matrix needs to be created through the makeCacheMatrix function in order to pull functions within that function.
## ex: 
##"zz" <- makeCacheMatrix()
##zz$setx(matrix(... (inversable vector), nrow, ncol))

cacheSolve <- function(x, ...) {
  inv <- x$getinvx()
  if((!is.null(inv))){
    print("getting the inverse")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinvx(inv)
  inv
  
        ## Return a matrix that is the inverse of 'x'
}

