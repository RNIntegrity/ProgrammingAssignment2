# programming assignment 2 for R Programming, by Isaac Hampton, submitted 18.3.15
# framework for the code was makeVector and cachemean from the assignment

# makeCacheMatrix defines an object that's matrix-esque and
# assigns some methods to it for cacheSolve to use later

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve first checks to see if the inverse of the "matrix"
# exists in memory and outputs it, otherwise finds the inverse
# and saves it to memory for later

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # check memory to see if the inverse exists; output and exit if so
  if(!is.null(m)) {
    message("Fetching saved inverse...")
    return(m)
  }
  
  # find the inverse, save it, and return it if the above check failed
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

