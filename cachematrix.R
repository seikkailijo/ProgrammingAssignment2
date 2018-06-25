## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
##rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
##write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object which is really a list containing a function to
###   set the value of the matrix
###   get the value of the matrix
###   set the value of the inverse matrix
###   get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix is the same), 
## "cacheSolve" function should get the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() #write the cached answer into the variable
  if (!is.null(i)) {
    message("getting cached data") #check if the cached answer is available, return it is there is some
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##to check the functions:
mat <- matrix(c(sample(1:100, 9)),3,3) #generate a matrix
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2) 
cacheSolve(mat2) #run once more to get the cached answer
