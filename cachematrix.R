## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: creates a matrix object
# cacheSolve: computes the inv of a matrix or return the cached valued if it exists

## Write a short comment describing this function

# Constructs a matrix object and defines 4 methods

makeCacheMatrix <- function(x = matrix()) { # x is an NA matrix by default
  invX = NULL
  set = function(y) { # put the matrix in the cache
    x <<- y # change the value of x defined in the parent function
    invX <<- NULL # reset inv to null
  }
  get = function() x # get the matrix x from the cache
  setInv = function(inv) invX <<- inv # put the inverse in the cache (defined in the parent function)
  getInv = function() invX # get the inv from the cache
  list(set = set, get = get, setInv = setInv, getInv = getInv) # return list of functions
}

## Write a short comment describing this function

# takes a cacheMatrix as argument
# computes the inverse if necesary
# returns the inverse

cacheSolve <- function(x, ...) { # x is a cacheMatrix
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInv()
  if(!is.null(inv)) { # inv has been calculated yet
    message("getting inverse from cached data")
    return(inv) # exit the function
  }
  # inv need to be calculated
  M = x$get() # get the matrix from the cach
  inv = solve(M) # cal the inv
  x$setInv(inv) # store the inv in the cache
  return(inv)
}
