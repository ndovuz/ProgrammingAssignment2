## The purpose of this function is to compute the inverse of a matrix, by relying on previous 
###computations of the inverse, if the result was stored in cache.
###This function is particularly useful when computing the inverse of matrix of large matrices.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #Require MASS library so that you can run 'ginv()' function to get the inverse of a matrix
  ##MASS used to provide additional flexibility that 'solve()' does not have
  require(MASS,quietly=TRUE,warn.conflicts=TRUE)
  #Load library to get all related MASS configurables
  library(MASS,quietly=TRUE,warn.conflicts=FALSE)
  
  #Initialize the return value, m, as NULL
  m <- NULL
  
  
  # Create a function that will reset the matrix and clear the cache. 
  ##This will become apparent in the CacheSolve function,
  ## and will prompt a recomputation of the inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Create a function that will return the matrix entered in the arguments
  get <- function() x
  
  #Recompute the inverse
  setinverse <- function(ginv) 
  {
    m <<- ginv
  }
  
  #Retrieve the inverse from the Cache
  getinverse <- function()
  {
    m
  }
  
  #The list that is returned by this function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function efficiently returns the inverse of x, if the inverse is stored in the cache

cacheSolve <- function(x , ... ) 
{
  
  #Retrieve Inverse from cache
  m <- x$getinverse()
  
  #Return inverse from cache if it is not NULL
  if(!is.null(m)  ) 
  {
    message("getting cached data")
    return(m)
  }
  
  #Retrieve matrix from argument if cache is empty, and compute Inverse
  data <- x$get()
  m <- ginv(data, ...)
  
  #Save Inverse into cache
  x$setinv(m)
  
  #return Inverse matrix
  return(m)
  
}




