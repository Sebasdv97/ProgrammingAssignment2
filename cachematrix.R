makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {#load a matrix into 'makeCacheMatrix'
    x <<- y #save x in other enviroment
    inv<<- NULL #reset inv in other enviroment
  } 
  get <- function() x #get the matrix
  setinv <- function(inverse) inv <<- inverse#set marix from what I want the inverse
  getinv<- function() inv #get matrix inv
  list(set = set, get = get,
          setinv = setinv,
          getinv = getinv) #create the list with matrix and inverse
  
}


cacheSolve <- function(x, ...) { #return the inverse matrix from the cache  or solve a matrix and then store the inverse matrix in the cache.
  inv <- x$getinv() #search the inv in cache memory
  if(!is.null(inv)) { #compare if the inv is in the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #saves the marix to be inverted in data (temporal memory)
  inv <- solve(data, ...) #calcultes the inverted matrix
  x$setinv(inv) # Saves the inverted matrix in the cache
  inv #shows result
