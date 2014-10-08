## The following function create a sort of extend matrix object.
## When the result of makeCacheMatrix is stored in a variable, 
## you can use the functions inside the list to access properties.
## variableName$functionName()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks if the inverse is cached in the cacheMatrix object.
## If it is not, it calculates it and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message('Getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x $setinverse(m)
  m
}

## A test function, which can be used to verify that it works.

test <- function(){
  sample <- rbind(c(1, -1/2), c(-1/2, 1))
  cachedMatrix <- makeCacheMatrix(sample)
  message("Matrix: ")
  print(cachedMatrix$get())
  message("Calling cacheSolve, inverted matrix is not cached yet.")
  invertedMatrix <- cacheSolve(cachedMatrix)
  
  message("Calling cacheSolve, inverted matrix is cached now as seen by \"Getting cached data\" which is printed inside the if in cacheSolve.")
  invertedMatrix <- cacheSolve(cachedMatrix)
  
  message("Inverted matrix: ")
  print(invertedMatrix)
  message("Matrix * InvertedMatrix equals: ")
  print(sample %*% invertedMatrix)
}
