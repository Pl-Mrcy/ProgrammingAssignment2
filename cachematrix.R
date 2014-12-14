## Following, a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(Y){
          X <<- Y
          inv <<- NULL
     }
     get <- function() X
     setInv <- function(inverse) inv <<- inverse
     getInv <- function() inv
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- X$getInv()
     if(!is.null(inv)){
          message("Getting Cached Data")
          return inv
     }
     data <- X$get()
     inv <- solve(data, ...)
     X$setInv(inv)
     inv
}
