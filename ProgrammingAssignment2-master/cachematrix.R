##function which makes the list of functions, which are described
##in the body of makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  ##makes the variable inv and sets it to NULL
  inverse <- NULL
  ##the set function takes whatever matrix is passed and assigns
  ##it to x, and sets inv to NULL,letting the cacheInverse function
  ##know we need to calculate the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##get will return the matrix x
  get <- function() x
  ##setinverse will receive the inverse and cache it to inv
  setinverse <- function(inverse) inv <<- inverse
  ##getinverse will return inv
  getinverse <- function() inv
  ##a list of the four functions that can be called
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##function which checks if the matrix has had the inverse
##calculated, and calculates it if it has not been
cacheInverse <- function(x, ...) {
  ##gets the inverse, which will be NULL if it hasn't been cached
  inv <- x$getinverse()
  ##if the inverse is not null, returns it with a 
  ##message and the function is done
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##otherwise we assign the matrix to the variable data, calculate
  ##the inverse of the matrix and calls the setinverse function
  ##to cache the calculated inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ##finally, it outputs the inverse
  inv
}