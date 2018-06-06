## My functions here will take advantage of R's lexical scoping rules and
## avoid repeated computation of the inverse of a matrix by caching its value


## This is how we create a cache matrix for storing matrix x, as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {             ##created function which takes in an invertible matrix as an argument
  
    i <- NULL                                           ##Initialised an object in this environment which contains nothing at time of design, but will store the value of whatever inverse is computed later on
    
    set <- function(y) {                                ##created a new function which can reset the input matrix x, and computed inverse value i, and start over again
    x <<- y                                             ##<<- implies an input argument is assigned to x in the PARENT ENVIRONMENT
    i <<- NULL                                          ##value of inverse in parent environment is set to Null, therefore RESET
  }
  
  
  get <- function() x                                   ##created a function which simply returns x, which is stored in the  parent environment of get(), which is the environment in which makeVector() is defined
  
  
  setinverse <- function(inverse) i <<- inverse         ##created a function which sets the value of i, as the value of another object inverse, in the parent environment
  
  
  getinverse <- function() i                            ## created a function which returns value of i(inverse), from parent environment
  
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ## Assigned each of the functions above as an element within a list(), and returned it to the parent environment
  ## Each element is now named, so that dollar operator can be used below instead of [[]]
}


## This function will check whether inverse has already been computed before or not, it yes, it serves up the value of the inverse stored as cached data
## else it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
      if(!is.null(i)) {                   ## If null, it will proceed below, compute, store as cached data and then return the inverse.
        message("getting cached data")
        return(i)
       }
  
  
    data <- x$get()
  
  
    i <- solve(data, ...)
  
  
    x$setinverse(i)
  
  
    i
  
}

##https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md This really helped a lot