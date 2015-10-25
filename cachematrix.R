## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
##                  that can cache its inverse.
## cacheSolve:      This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

## This function attaches a list of functions and its inverse to the matrix object
makeCacheMatrix <- function(x = matrix()) 
{
  ## set the inverse to null.
  xinverse = NULL
  ## set the matrix value
  set <- function(y)
  {
    # set the value in other environment
    x <<- y 
    xinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
  {
    xinverse <<- inverse
    inverse
  }
  getinverse <- function() xinverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Calculates a matrix that is the inverse of 'x' or returns the previously calculated
## matrix stored in cache.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse))
        {
          message("Getting inverse from cache")
          return(inverse)
        }
        data <- x$get()
        message("Computing inverse")
        inverse <- solve(data,...)
        x$setinverse(inverse)
}
