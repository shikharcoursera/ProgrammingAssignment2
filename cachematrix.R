#this function makes the cache matrix
#take matrix as an input
makeCacheMatrix <- function(a = matrix()) {
  inverseMatrix <- NULL
  
  
  setMatrix <- function(y) {
    a <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() a                             
  setInverse <- function(inverse) inverseMatrix <<- inverse  
  getInverse <- function() inverseMatrix                    #get the value of the invertible matrix 
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}
##function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.


cacheSolve <- function(a, ...) {
  
  #taking value from makeCachMatrix
  inverseMatrix <- a$getInverse()
  if(!is.null(inverseMatrix)) {                       
    message("Getting Cached matrix")   
    return(inverseMatrix)                             
  }
  
  
  
  
  MatrixData <- a$getMatrix()                      
  inverseMatrix <- solve(MatrixData, ...)               #solve function to inverse the matrix
  a$setInverse(inverseMatrix)                          
  return(inverseMatrix)                #return invertible matrix                 
   ## Return a inverse matrix
}
