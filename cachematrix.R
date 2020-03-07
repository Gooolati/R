## Our goal is to cache a matrix
## i.e., store the memory address of the
## matrix in a variable that we can 
## access if needed

##the function below first defines the
##setter and the getter functions
##then it defines the setter for the inverse
##then the getter for the inverse

##so first it 'sets/gets' the matrix, and then
##'sets/gets' the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## Write a short comment describing this function

#first it gets the inverse using the above getter
#checks to see if it's in the cache (which was set above)
#if it's not in the cache, it gets the inverse and then
#stores the inverse in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

