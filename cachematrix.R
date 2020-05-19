# R PROGRAMMING 
# PROGRAMMING ASSIGNMENT 2: LEXICAL SCOPING

# makeCacheMatrix is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse 
# 4. get the value of the inverse

# CREATE AN R OBJECT THAT STORES A MATRIX AND ITS INVERSE
makeCacheMatrix<- function(x = matrix()) {
  
  # Sets me to a NULL as placeholder for future value of inverse
  inv <- NULL
  
  # defines a function to set the matrix (x) 
  # to a new vector (y) and resets the inverse (m) to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # returns the matrix x
  get <- function() x
  
  # sets the inverse (x) to inverse
  setinv <- function(solve) inv <<- solve
  
  # returns the inverse m
  getinv <- function() inv
  
  # returns the 'special matrix' containing all of the
  # functions just defined
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# REQUIRES AN ARGUMENT RETURNED BY makeCacheMatrix() IN ORDER
# TO RETRIEVE THE INVERSE FROM THE CACHED VALUE THAT IS 
# STORED in makeCacheMatrix() OBJECT's ENVIRONMENT
# Without cacheSolve(), the makeMatrixCache() function is
# incomplete because cacheSolve() is required to populate
# or retrieve the inverse from an object of type makeMatrixCache()

# this checks if you have the inverse of the matrix of interest
# if these exist then you do not need to calculate and can use
# the cache variable

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}