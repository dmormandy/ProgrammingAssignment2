## Two Functions to create a vector containing functions to store a numeric vector 
## and cache its inverse

## makeCacheMatrix takes a numeric matrix and returns in a list functions to:
## 1. Set the value to memory
## 2. Get the value from memory
## 3. Set the value of the inverse of the matrix to memory
## 4. Get the value of the inverse of the matrix from memory


makeCacheMatrix <- function(x = matrix()) {
      ## Initialise the inverse to NULL
  inv_x <- NULL
      ## Create function "set" to set x to input value and inv to NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
      ## Create function "get" to get value of x
  get <- function() x
      ## Create function "setinverse" to set inv_x to the inverse of x
  setinverse <- function(x) inv_x <<- solve(x) 
      ## Create function "getinverse" to return value of inv_x  
  getinverse <- function() inv_x 
      ## Put all (named) functions into a list and return
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of a numeric matrix by:
## 1. Checking to see if the inverse has already been calculated. If so, it gets the mean from the cache 
## 2. Otherwise, it calculates the inverse of the numeric matrix 
##    and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(flist, ...) {
        ## Inputs is the list of functions "flist" (created by makeCacheMatrix) 
        ## Attempt to first get the inverse matrix
  inv_x <- flist$getinverse()
        ## If inverse matrix already exists then return it
  if(!is.null(inv_x)) { 
    message("getting cached data")
    return(inv_x) 
        ## Otherwise set the inverse and return it
  } else 
    data <- x$get()
    inv_x <- flist$setinverse(data)
    message("setting inverse and storing in cache")
    return(inv_x)
}
