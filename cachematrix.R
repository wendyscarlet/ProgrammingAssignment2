## Creates a special "matrix" object that can cache its inverse. It is a list of functions that allow to set and get the value of the matrix
##and the value of the inverse matrix

makeCacheMatrix <- function(originalMatrix = matrix()) {
    inversedMatrix <- NULL
    
    ## Set the value of the matrix
    set <- function(y) {
        originalMatrix <<- y
        inversedMatrix <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() originalMatrix
    
    ## Set the value of the inverse of a matrix
    setInversed <- function(matrix) inversedMatrix <<- matrix
    
    ## Get the value of the inverse of a matrix 
    getInversed <- function() inversedMatrix
    list(set = set, get = get,
         setInversed = setInversed,
         getInversed = getInversed)
}

## Return a matrix that is the inverse of 'matrix'
inverseMatrix <- function(matrix, ...) { 
    inversedMatrix <-matrix$getInversed()
   
    ##If  inversedMatrix is not null return the value stored in cache
    if(!is.null(inversedMatrix)) {
        message("getting data from cache")
        return(inversedMatrix)
    }
    ##Else we have to calculate the inverse of the matrix
    message("calculating the inverse of the matrix")
    ##First we get the matrix to invert
    data <- matrix$get()
    ##Invert the matrix using the solve function
    inversedMatrix<- solve(data, ...)
    ##Store the inversed matrix in the cache
    matrix$setInversed(inversedMatrix)
    inversedMatrix
}
