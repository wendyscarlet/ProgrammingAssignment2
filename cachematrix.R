## Creates a special "matrix" object that store a matrix and  cache its inverse. 
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
    data <- matrix$get()              ##First we get the matrix to inverse   
    inversedMatrix<- solve(data, ...) ##Inverse the matrix using the solve function
    matrix$setInverse(inversedMatrix) ##Store the inversed matrix in the cache
    inversedMatrix
}
