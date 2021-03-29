#cachematrix

#First function: makeCacheMatrix. This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  # store inverse value
  inverse <- NULL
  # set and get functions similar to the cachevector example
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  get <- function() matrix
  # inversion instead of mean (opposed to cachevector example)
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  
  # Returns the special "matrix", analogous to the cachvector example.
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}
#Second function: cacheSolve. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$get_inverse()
  if(!is.null(inverse)) {
    message("loading cached data")
    return(inverse) #returning the inversed value from cache
  }
  data <- matrix$get()
  inverse <- solve(data, ...)
  matrix$set_inverse(inverse)
  inverse #returning the reversed value if not already in cache
}