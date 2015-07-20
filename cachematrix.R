# Create the pair of functions `makeCacheMatrix` and `cacheSolve` to:
#
# 1. Create a special "matrix" that its inverse can be cached
# 2. Compute the inverse (retrieve from cache if already cached) of the obove special "matrix"



# The function `makeCacheMatrix` creates a special "matrix", which is a list containing a function to:
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


# The function `cacheSolve` calculates the inverse of the special "matrix"
# created with the above function. It first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) { # alread cached
    message("getting cached data")
    return(m)
  }
  # otherwise we compute & store the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
