## This code is used for the computation of caching the inverse
##of a matrix rather than coputing it repeatedly.

#The first function creates a special "matrix" object
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL         #Set m to NULL for storing of inverse data later
  set <- function(y) { # set the value of the vector
    x <<- y
    m <<- NULL      #Setting value of inverse(m) to null if entered new matrix
  }
  get <- function() x #Get the value of the vector
  setinverse <- function(inverse) m <<- inverse #set the value of the inverse(m)
  getinverse <- function() m # get the value of the inverse(m)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

##The second function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. 
##if the iverse has already been calculated,
##the function will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ##if the iverse has already been calculated,
  ##the function will retrieve the inverse from cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m #Print value of the cache inverse of the matrix
}
