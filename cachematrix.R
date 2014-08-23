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
    setInversedMatrix <- function(matrix) inversedMatrix <<- matrix
    
    ## Get the value of the inverse of a matrix 
    getInversedMatrix <- function() inversedMatrix
    list(set = set, get = get,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}

## Return a matrix that is the inverse of 'x'
setInversedMatrixInCache <- function(m, ...) { 
    inversedMatrixCache <-m$getInversedMatrix()
   
    ##If the inversedMatrix is not null return the value saved in cache
    if(!is.null(inversedMatrixCache)) {
        message("getting data from cache")
        return(inversedMatrix)
    }
    ##Else we have to calculate the inverse of the matrix
    ##First we get the matrix to invert
    data <- m$get()
    ##Invert the matrix using the solve function
    inversedMatrix<- solve(data, ...)
    ##Store the inversed matrix in the cache
    m$setInversedMatrix(inversedMatrix)
    inversedMatrix
}
