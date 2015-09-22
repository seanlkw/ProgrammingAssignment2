## Objective of this function is to be able store values of a Matrix as
## cache fo faster operations. 


makeCacheMatrix <- function(x = matrix()) {

  # stores the cached value
  # initialize to NULL
  
  m <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
    # invert the matrix and store in cache
  setInv <- function(inv) m <<- inv
  # get the inverted matrix from cache
  getInv <- function() m
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# Calculates th3 inverse of the matrix stored in Cache
# create through "makeCacheMatrix" function above
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  ## attempt to get the inverse of the matrix stored in cache
    m <- x$getInv()
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    # set and return inverse of matrix
    m <- solve(data, ...)
    # set inverted matrix in cache
    x$setInv(m)
    m

}

#Execution:
# > source(xxxxxxxxxxxxxxx)
# > a <- makeCacheMatrix(matrix(1:4, 2, 2)) "for creating matrix in caching it"
# > cacheSolve(a) "inverse matrix"
#         [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5