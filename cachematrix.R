## This assignment is to write a pair of functions 
## that cache the inverse of a matrix.


## This function creates a special matrix object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y) 
    {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated and hasn't changed, 
## cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  inverse <- x$get_inverse()
  if(is.null(inverse)) 
  {
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
  }
  else
  {
    message("getting cached data")
  }
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
