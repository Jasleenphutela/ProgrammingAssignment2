
#x as input matrix
# s value to null
makeCacheMatrix <- function(x = matrix()) {
         s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

 ## ---------------Checking the program------------------------
 ## m <- matrix(rnorm(16),4,4)
 ## m1 <- makeCacheMatrix(m)
 ## cacheSolve(m1)

 ## [,1]       [,2]       [,3]       [,4]
 ## [1,] -0.1653269  0.2592203  0.6176218 -0.7520955
 ## [2,]  0.2828334 -0.1853499  0.4511382  0.2094365
 ## [3,]  0.1434840  1.0413868 -0.3550853 -0.3261154
 ## [4,]  0.1793583 -0.4252171 -0.4371493 -0.1749830


