## caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) { ##set the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x ## get the value 
  setinverse <- function(solve) i <<- solve ##set the inverse
  getinverse <- function() i ## get the inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## return the result
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) { 
  
  i<- x$getinv()##load
  if(!is.null(i)) {## if the cache is not empty then use that instead of recompute.
    message("Returning cached data") ## notify user of cached data used.
    return(i) ## retuen cached value
  }
  ##else we need to calculate the value
  datatoinv <- x$get() ## load it in
  i<- solve(datatoinv, ...) 
  x$setinverse(i) ##iinverse it
  i ## return the value
  
}