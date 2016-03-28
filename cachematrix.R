## Matrix inversion, especially when dealing with large matrices, can be 
## costly in processing time and memory. 

## The makecacheMatrix and cacheSolve functions are used to store an inverted copy of 
## a matrix (makeCacheMatrix) and return the inversion of matrix(x) from memory if x 
## is a square, invertible matrix that has already been inverted (cacheSolve)

## makeCacheMatriX stores the inverse of matrix (in a list) to prevent
## the recalculation of the matrix inversion with every iteration


makeCacheMatrix <- function(x = matrix()) {
  
  
  i <- NULL
  ## Set the matrix / function 
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the matrix
  getmatrix <- function() x
  
  ## create the function [stored in setinverse] to invert the matrix 
  ## using solve() by passing a (as m) and excluding b
  setinverse <- function(solve) i <<- solve
  
  ## get the inverted matrix via new function
  getinverse <- function() i
  
  ## use a list to store the new function content 
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve return the inverse matrix of x by either returning the cached inversion of the matrix
## or computing the inverse if the inverted matrix is not available

cacheSolve <- function(x, ...) {
  
  ## Return the inverted matrix that is the inverse of 'x' [makeCacheMatrix]   

  i <- x$getinverse()
  
  ## if inverted matrix has been cached, use the cached version 
    if(!is.null(i)) {
        message("getting inverted matrix")
        return(i)
    }
    
        ## if there is no cached inverted matrix, return the matrix and invert it here
      
        ## return matrix      
        matrix_data <- x$getmatrix()
        
        ## invert matrix and set inversion (next pass whould skip this else condition block)
        i <- solve(matrix_data)
        x$setinverse(i)
        i
    
}
