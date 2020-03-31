makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y #save x in othr enviroment
    inv<<- NULL #reset inv in other enviroment
  } 
  get <- function() x #get the matrix
  setinv <- function(inverse) inv <<- inverse#set marix sobre la que quiero la inversa
  getinv<- function() inv #get matrix inv
  list(set = set, get = get,
          setinv = setinv,
          getinv = getinv) #create the list
  
}


cacheSolve <- function(x, ...) {
  inv <- x$getinv() #search the inv in cache memory
  if(!is.null(inv)) { #compare if the inv is in the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #saves the marix to be inverted in data
  inv <- solve(data, ...) #calcultes the inverted matrix
  x$setinv(inv) # Saves the inverted matrix in the cache
  inv #shows result
}
