
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
    setInversedMatrix <- function(m) inversedMatrix <<- m
    
    ## Get the value of the inverse of a matrix 
    getInversedMatrix <- function() inversedMatrix
    list(set = set, get = get,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}

