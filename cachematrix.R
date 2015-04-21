## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##assume matrix x has inverse
  i <- NULL # init i,i is the inverse of x
  set <- function(y) {
    x <<- y
    i <<- NULL ##matrix is changed, reset inv to NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv #cache inv value into i
  getinv <- function() i #get inv value i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
 
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data)
  x$setinv(i) #store the inv number in chache
  i
        ## Return a matrix i that is the inverse of 'x'
}
