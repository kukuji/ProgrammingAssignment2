## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # init the defautl value of m to NULL
  y <- NULL # init the defautl value of y to NULL
  
  #set the value of the matrix
  setmatrix <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  m <- x$getinverse() # if an inverse has already been calculated it's assigned to m
  if(!is.null(m)){ 
    if(x$setmatrix() == x$getmatrix()) { # if the matrix is equall to cached matrix then return the cached one
      
      return(m)
    }
    
    #if it's not cached 
    y <- x$getmatrix() 
    x$setmatrix(y)  # here we cache the matrix 
    m <- solve(y, ...) 
    x$setinverse(m)
    
    m # return the inverse
}
