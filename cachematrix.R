## Programming Assignment 2 for the Coursera R course.
## 2 functions (with embedded functions) to demonstrate object caching
## and lexical scoping. In this case caching a matrix object.
##    (NOTE: Many assumptions are made with this assignment, so there are 
##    some elements, such as verification and error handling, not
##    implemented. There may be ways to implement with fewer functions,
##    however the below is matching the example from the course.)

## makeCacheMatrix returns a list of functions to use on a matrix object

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {                ## Sets/Resets cached object values
            x <<- y
            m <<- NULL
      }
      get <- function() x                 ## Returns the cached matrix
      setinverse <- function(inverse) m <<- inverse   ## Caches passed value
      getinverse <- function() m                      ## Returns cached inverse
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)       ## List returned by makeCacheMatrix
}

## cacheSolve will return a matrix that is the inverse of 'x'
## using the solve() function (as per assignment instructions, we assume
## the matrix can be inverted, i.e.: it is not singular). 
##
## cacheSolve will first check if a cached version of the matrix inverse
## exists, and will return that value if it exists. Otherwise it will 
## run the solve() function, cache the result (in case this function is
## is called in the future), and then return the result.

cacheSolve <- function(x, ...) {
      ## Getting m object to check if cached value already exists
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)         ## returning value of the cache
      }
      ## If cache doesn't exist, now getting matrix object to invert...
      data <- x$get()
      m <- solve(data, ...)   ## inverting matrix
      x$setinverse(m)         ## creating cache object of inverted matrix
      m                       ## returning the inverse
}
