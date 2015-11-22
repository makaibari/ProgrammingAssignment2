## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a cached version of a matrix and its inverse, if already calculated
  
  invX <- NULL
  set <- function(y){ ## Cache the matrix
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInv <- function(m) invX <<- m ## Cache already calculated inverse
  getInv <- function() invX ## return the cached matrix inverse
  list(set = set, get= get, setInv = setInv, getInv=getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Attempts to get the inverse from cache, if already calculated for the same matrix
  ## If not, calculates and caches for future use 
  
  invX <- x$getInv() ## get inverse if already available in cache
  if (!is.null(invX)){
    message("Getting cached data")
    return (invX)
  }
  # If not, calculate inverse
  data <- x$get()
  invX <- solve(x$get())
  
  #Cache the calculated inverse for future use
  x$setInv(invX)
  invX

}
