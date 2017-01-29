
##Note: have changed set, get list elements from the makeVector sample to be 
##setorig, getorig -- so swapping in new matrix will use testVector$setorig 
##rather than testVector$set

##To create a vector that can store a matrix and its inverse
##create 'new' matrix, make sure m (cache) is empty
makeCacheMatrix <- function(x = matrix()) {
    
    m <- matrix()
    m <- NULL
   
    ##set input to x, reset cache 
    setorig <- function(y) {
        x <<- y
        m <<- NULL
      }
  
    ##retrieve original (input) matrix value
    getorig<- function() x 
    
    ##calculate inverse matrix
    setinverse <- function(invmatrix) m <<- invmatrix

    
    ##retrieve inverse matrix
    getinverse <- function() m
    
    ##output is list of 4 elements to be used by cacheSolve    
        list(setorig = setorig, getorig = getorig,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  

## To return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

    ## Look for cached inverse; if exists, notify user and return
  
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
  
    ##If not cached, calculate inverse, store as m and return
    
    data <- x$getorig()
    m <- solve(data)
    x$setinverse(m)
    m
  }
  

