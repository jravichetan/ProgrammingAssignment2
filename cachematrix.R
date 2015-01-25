# Below functions - Compute [makeCacheMatrix] and cache [cacheSolve] the inverse of matrix

# This function computes a special "matrix" object that cache's its inverse
makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

# This function computes the inverse of matrix created by makeCacheMatrix only when inverse is not present in cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}