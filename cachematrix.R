## Write a short comment
# makeCacheMatrix is a function that takes a matrix as input. 
# It initializes two objects: x = matrix() and m <-NULL.
# 4 functions：
# 1. set: The function takes a generic variable called y.
# x <<-y and m <<- NULL are assigned to the parent environment.
# m is cleared if cacheSolve function was previously executed.
# 2. get: from the parent environment of makeCacheMatrix, x is obtained
# 3. setinverse: m is defined in the parent environment. We can access once the inverse of the matrix m is executed by setinverse()
# 4. getinverse: it retrieves the inverse of the matrix from the parent environment.
# Example:
# m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# a<-makeCacheMatrix(m)
# cacheSolve(a)
# a$set(m)
# cacheSolve(a)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write short comment
# This function is required to populate or retrieve the inverse 
# of the matrix from an object of type makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

