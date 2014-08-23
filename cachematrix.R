## Creates a special "matrix" object that can cache its inverse. It is a list of functions that allow to set and get the value of the matrix
##and the value of the  matrix inverse

makeCacheMatrix <- function(originalMatrix = matrix()) {
    inversedMatrix <- NULL
    
    ## Set the value of the matrix
    set <- function(y) {
        originalMatrix <<- y
        inversedMatrix <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() originalMatrix
    
    ## Set the value of the  matrix inverse
    setInverse <- function(matrix) inversedMatrix <<- matrix
    
    ## Get the value of the matrix inverse
    getInverse <- function() inversedMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Stores in the cache and returns the matrix inverse 
cacheSolve <- function(matrix, ...) { 
    inversedMatrix <-matrix$getInverse()
   
    ##If we have the matrix inverse in the cache
    if(!is.null(inversedMatrix)) {
        message("getting data from cache")
        return(inversedMatrix)
    }
    ##Else we have to calculate the matrix inverse
    message("calculating the matrix inverse")
    ##First we get the matrix to inverse
    data <- matrix$get()
    ##Inverse the matrix using the solve function
    inversedMatrix<- solve(data, ...)
    ##Store the inversed matrix in the cache
    matrix$setInverse(inversedMatrix)
    inversedMatrix
}
