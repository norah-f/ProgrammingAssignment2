## cachematrix2.R:
## The following functions calculate and store the inverse of a regular matrix.
## Matrix for which the inverse was calculated also stored in cache.


## makeCacheMatrix: manages caching operations, no calculation carried out here.
## return value is a list of 4 functions as follows:
##    set - store the new matrix in cache; parameter is the matrix to be inverted
##    get - read the matrix from cache
##    setinv - store the inverse value in cache (used in the next function where inverse calculated);
##             parameter is the inverse matrix
##    getinv - read iverse matrix from cache. NULL is stored when inverse matrix not yet stored in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL        
  set <- function(y) {
    x <<- y        
    m <<- NULL     ## delete the previous inv.matrix from glob env/cache.
  }
  get <- function() x  
  setinv <- function(inv) m <<- inv 
  getinv <- function() m  
  
  ## this is the return "value" of makeCacheMatrix, components still functions with 1 param
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: checks if inverse was already stored in cache
##     a) if yes, reads value from cache;
##     b) if not calculates and stores the inverse with the aid of setinv() 
## parameter should be the object which store the result of makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()    ## pull out the inverse from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ## if inverse not yet in cache m is NULL not a matrix -> is.null can be used
  }
  data <- x$get() 
  m <- solve(data) 
  x$setinv(m) 
  m   ## return value is the inverse  
}
