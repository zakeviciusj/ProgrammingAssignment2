## We will first create a matrix inverse cache and then solve the inverse for a given matrix
## while taking cache if available

## This one creates a matrix inverse cache

makeCacheMatrix <- function(x = matrix()) {

  # Let's create an inverse placeholder first
  inv <- NULL
  # Now set the matrix
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  # And get the matrix
  get <- function() x
  # Here we are setting the matrix inverse
  setinverse <- function(solve) inv <<- solve
  # And getting the matrix inverse from cache
  getinverse <- function() inv
  # Finally, creating a list to combine the above
  list (set = set, get = get,
        setinverse = setinverse, 
        getinverse = getinverse)
  
}


## OK, and this one is for calculating the inverse, but taking the cached result if available

cacheSolve <- function(x, ...) {
  
  # First we retrieve the inverse if it is available
  inv <- x$getinverse()
  # We now check if what we retrieved is not empty
  if (!is.null(inv)) {
  # If it is not
  # We first display a message to inform the user about this
    message ("retrieved result is cached")
  # And then return the cached result
    return(inv)
  }
  # And otherwise we get the matrix first
  data <- x$get()
  # Calculate the inverse for this matrix
  inv <- solve(data, ...)
  # We set the new inverse to the cache
  x$setinverse (inv)
  # And finally return the inverse
  inv
    
}
