## These two functions are used to calculate and cache the inverse of a matrix


## makeCacheMatrix creates special vector that is a list used to 
## calculate and cache the inverse of a matrix. If the inverse has
## been cached it will just return the cached entry.
## This function uses the special operand <<- to make objects available
## outside of their current environment.

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      
      set <- function(y) {
            x <<- y         
            m <<- NULL
      }
      
      get <- function() x
      
      setmatrixinv <- function(matrixinv) m <<- matrixinv
      
      getmatrixinv <- function() m
      
      list(set = set,
           get = get,
           setmatrixinv = setmatrixinv,
           getmatrixinv = getmatrixinv)

}


## cacheSolve uses the vector from makeCacheMatrix to return the passed 
## matrix's inverse. It checks to see if the inverse has already been calculated first.
## If it has been calculated it will return the cached entry. Otherwise it will
## calculate the inverse with the solve function and also save the
## result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getmatrixinv()
      
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()

      m <- solve(data, ...)
      
      x$setmatrixinv(m)
  
      m

}
