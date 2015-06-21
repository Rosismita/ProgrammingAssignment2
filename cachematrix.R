# This script calculates inverse of a Matrix and
# checks & returns value if the inverse is available in cache, else it calculates the inverse again

#The first function, makeMatInv creates a special "vector", which is really a list containing a function to
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the invmat
#4. get the value of the invmat

makeMatInv <- function(x = matrix()) {
  m <- NULL  
  get <- function() x
  setinv <- function(invmat) m <<- invmat  # stores the argument ('value') in global vector 'm'
  getinv <- function() m  # returns the global value of vector 'm'
  list(get = get,
       setinv = setinv,
       getinv = getinv)#defining names for functions
}

#The following function calculates the Inverse of a Matrix of the special "vector" created with the above function. 
#However, it first checks to see if the Inverse has already been calculated. If so, it gets the 
#Inverse Matrix from the cache and skips the computation. Otherwise,it calculates the Inverse of the Matrix 
#and sets the value of the Inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  m <- x$getinv() 
  if(!is.null(m)) { #checking if the value of 'm'(value of inverse Matrix) is available in cache.
    message("getting cached data")
    return(m) #return the value of 'm' which is already available in cache
  }
  data <- x$get() #Storing the value of Matrix
  m <- solve(data, ...) # Calculates and returns the value of inverse Matrix in 'm'
  x$setinv(m) #Set the value(inverse Matrix) in 'm'
  print(m) 
}