## To save time and computing power by using invertex matrices, the following functions 
## (a) create cached variable that contain value of inversed matrix and
## (b) check if particular matrix was inversed and cached and if yes, the returns it. If not, then inversion is calculated.
## functions do

## The following function creates special variable (list containing another function to store
##    (a) value of the matrix and 
##    (b) cached value of inverted matrix)

makeCacheMatrix <- function(x = matrix()) {
  ci <- NULL 
  set <- function(y) { 
    x <<- y
    ci <<- NULL
  }
  get <- function() x
  setInv <- function(solve) ci <<- solve
  getInv <- function() ci
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## The function below checks if particular matrix was inversed and cached and if yes, returns it. 
##  If not, then inversion is calculated.

cacheSolve <- function(x, ...) {
  ci <- x$getInv()
  if(!is.null(ci)) {
    message("..getting cached data..")
    return(ci)
  }
  message("..calculating data..")
  data <- x$get()
  ci <- solve(data, ...)
  x$setInv(ci)
  ci
  
    ## Return a matrix that is the inverse of 'x'
}