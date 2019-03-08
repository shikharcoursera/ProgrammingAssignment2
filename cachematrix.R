makeCacheMatrix <- function(a = matrix()) {
  inverseMatrix <- NULL
  
  
  setMatrix <- function(y) {
    a <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() a                             
  setInverse <- function(inverse) inverseMatrix <<- inverse  
  getInverse <- function() inverseMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}



cacheSolve <- function(a, ...) {
  
  #taking value from makeCachMatrix
  inverseMatrix <- a$getInverse()
  if(!is.null(inverseMatrix)) {                       
    message("Getting Cached matrix")   
    return(inverseMatrix)                             
  }
  
  
  MatrixData <- a$getMatrix()                      
  inverseMatrix <- solve(MatrixData, ...)             
  a$setInverse(inverseMatrix)                          
  return(inverseMatrix)                               
  
}
