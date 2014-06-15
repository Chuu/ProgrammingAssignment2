## Contains functions to create and manipulate a special Matrix object that 
## can cache it's inverse.
##

##Creates a matrix that can cache it's inverse.  Uses set() and get()
##to get/set underlying values.
makeCacheMatrix <- function(x = matrix()) {
  
  #Cached mean
  m_cachedInverse <- NULL
  
  #Underlying Matrix get/set
  set <- function(matrix)
  {
    x <<- matrix
    m_cachedInverse <<- NULL
  }
  get <- function() { x }
  
  #Cached inverse get/set.  This *really* shouldn't be 
  #part of a public interface . . . but the assignment's
  #API requires it
  setInverse <- function(inverse) { m_cachedInverse <<- inverse }
  getInverse <- function() { m_cachedInverse }
  
  list(set = set, get=get,
       setInverse = setInverse, getInverse=getInverse)
}


##Returns the inverse when passed an instance of the special matrix 
##type above.  Will use cached value if previously calculated.
cacheSolve <- function(x, ...) {
  
  retVal <- x$getInverse()
  
  if(!is.null(retVal))
  {
    message("Using cached value")
    return(retVal)
  }
  
  retVal <- solve(x$get())
  x$setInverse(retVal)
  retVal
}
