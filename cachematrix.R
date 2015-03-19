## This code contains two functions, makeCachMatrix and cacheSolve. The purpose is to take a matrix,
## calculate its inverse, and cache this inverse. Then if this inverse is needed in the future it
## will not need to be calculated.
##
## The two functions are:
##    makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##    cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##        If the inverse has already been calculated (and the matrix has not changed), 
##        then cacheSolve should retrieve the inverse from the cache.
##        IF the inverse has not already been calculated, then calculate the inverse


## This function creates a special "matrix" object tha can cache a matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  # Set the inverse to null
  inverse <- NULL
  # Print the working environment
  print(environment())
  evn <- environment()
  # Print the parent environment
  print(parent.env(evn))
  # Define the fuction "set", which takes sets the value of the matrix
  set <- function(y = matrix()) {
    x <<- y
    inverse <<- NULL
  }
  # Define "get", which holds the value of the matrix
  get <- function() x
  # Define setinverse, which sets the value of the inverse of the matrix
  setinverse <- function(inv) inverse <<- solve(x)
  # Define getinverse, which gets the value of the inverse of the matrix
  getinverse <- function() inverse
  # Define getevn, which gets the environment
  getevn<- function() environment()
  # Store set, get, setinverse, getinverse, and getevn in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getevn = getevn)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Define inverse as the result of getinverse() from makeCacheMatrix
  inverse <- x$getinverse()
  # If the inverse has already been calculated, retreive it from the cache
  # If the inverse has not already been calculated, calculate it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  # Print the inverse
  inverse
}