
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

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    inversedMatrix <-x$getInversedMatrix()
    ##If the inversedMatrix is not null return the value saved in cache
    if(!is.null(inversedMatrix)) {
        message("getting data from cache")
        return(inversedMatrix)
    }
    data <- x$get()
    inversedMatrix <- solve(data, ...)
    x$setInversedMatrix(inversedMatrix)
    inversedMatrix <- x$getInversedMatrix()
    if(!is.null(inversedMatrix)) {
        message("getting data from cache")
        return(inversedMatrix)
    }
    data <- x$get()
    inversedMatrix<- solve(data, ...)
    x$setInversedMatrix(inversedMatrix)
    inversedMatrix
}
