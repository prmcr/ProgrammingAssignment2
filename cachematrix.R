
## Returns a list of first class objects to store and retrieve the matrix `x` as well as the cached inverse value `m`
## 

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


## cacheSolve is an alternate function for `solve` that 
##   1. takes as argument the list returned by `makeCacheMatrix` 
##   2. returns the stored inverse matrix *if* the argument's `getInverse` object does not return a NULL *else*
##   3. computes the inverse using `solve` and stores it for further use


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# X <- matrix(rnorm(25),5,5)
# Y <- makeCacheMatrix(X)
# cacheSolve(Y)
# cacheSolve(Y) # returns the cached inverse the second time cacheSolve(Y) is called
