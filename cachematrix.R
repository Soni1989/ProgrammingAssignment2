## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the Inverse of matrix
  setmatrix <- function(matrix) m <<- matrix
  ## get the value of the Inverse of matrix
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## it will return inverse of the matrix using solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
  
}
