## These functions will create a matrix object that can cache its inverse.

##makeCacheMatrix creates a matrix with a function to 
##set the value(s) of the matrix
##get the value(s) of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL 
  set <- function(y) {                                        
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x                                         
  setinverse <- function(inverse) inversematrix <<- inverse   
  getinverse <- function() inversematrix                      
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##CacheSolve calculates the inverse of the matrix
##checks to see if the inverse has already been calculated
##if, so it gets the inverse from the cache and tells the user "getting cached data"
##otherwise, it calculates the inverse of the matrix, returns a matrix that is the inverse of 'x' and
##sets the value of the inverse matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {                              
  inversematrix <- x$getinverse()                             
  if(!is.null(inversematrix)) {                               
    message("getting cached data")                            
    return(inversematrix)                                    
  }
  data <- x$get()
  inversematrix <- solve(data,...)
  x$setinverse(inversematrix)                                 
  inversematrix
}
      
