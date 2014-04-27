## cachematrix.R enables us to calculate, cache, and retrieve 
## a matrix and it's inverse to prevent recalculation.

## makeCacheMatrix() sets a matrix in cache memory for quick 
## retrieval and calculation. 

makeCacheMatrix <- function(x = matrix()) {
  # clear value of m
  m <- NULL
  
  # "sets" x (higher level environment) to the inputted value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # defined function to retrieve (get) x
  get <- function() x
  
  # solves the matrix inverse and stores it as m
  solveInv <- function(solve) m <<- solve
  
  # retrieves the inverse if available
  getInv <- function() m

  # list of available operators. set/get matrix, and 
  # set/get inverse solution
  list(set = set, get = get, 
       solveInv = solveInv, getInv = getInv)
}


## cacheSolve() returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  # set m to the matrix set in makeCacheMatrix()
  m <- x$getInv()
  # if there is already a value in m, just retrieve that instead
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if there is no inverse-m calculated in makeCacheMatrix, 
  # then we solve it here and return the value
  data <- x$get()
  m <- solve(data, ...)
  x$solveInv(m)
  m
}

## example matrix: x <- matrix(c(2,4,2,2),2)
## inverse should be: (-0.5,1.0,0.5,-0.5)