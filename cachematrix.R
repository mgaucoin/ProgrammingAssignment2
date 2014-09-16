## This function will take a matrix x and $$$$ it.
## If no matrix is passed when using the makeCacheMatrix function
## the function will default to an empy matrix
## Note: you will want to assign makeCacheMatrix to a new variable which can then 
# be used in CacheSolve e.g. 
# mat <- c(2,3,1,7,2,1,9,1,5)
# mat <- matrix(mat,3,3)
# M_1 <-makeCacheMatrix(mat)
# cacheSolve(M_1)
# another e.g.
# mat2 <- c(1,3,7,8)
# mat2 <- matrix(mat2,2,2)
# M_2 <-makeCacheMatrix(mat2)
# cacheSolve(M_2)


makeCacheMatrix <- function(x = matrix()) {

## First, make m NuLL to clear any previous values associated with m
## assigning NULL to m will make m NULL in this environment
  m <- NULL
  
## Next, get is a function that is defined when you run makeCacheMatrix;
## this puts the matrix you want to do an operation on into a function 
## environment
  get <- function() x

## setinverse is a function you call upon in order to commit the inverse to memory;
  setinverse <- function(inverse) m <<- inverse

## getinverse is the function you call upon to see if the inverse was previously set
## if the inverse was set using setinverse, m becomes the inverse and because we used
## <<- it now becomes available to this other function environment created by get
## otherwise m is NULL
getinverse <- function() m

## the following is an output that is a list.
  list(get = get, setinverse = setinverse, getinverse = getinverse)
}

## CacheSolve will calculate or retrieve the inverse of a matrix x
## You will want to pass the variable you created when you ran makeCacheMatrix
## to cacheSolve (see comments at start of file)
cacheSolve <- function(x) {
  
##  inv is being defined using the getinvers function we created with the makeCacheMatrix function 
  inv <- x$getinverse()
## inv will either be NULL or the stored inverse that was placed in the getinverse after using the 
## setinverse function; the next conditional statement is used to let you know if the inv is being calculated
## or retrieved from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## if the inverse does not exist i.e. inv = NULL, then we will use the get function to retrieve
## the matrix that we want to perform the operation on
  data <- x$get()
## the operation we want to perform on the matrix is "solve()" which will give us the inverse 
  inv <- solve(data)
## now that the inverse is calculated, store it in cache using the setinverse function
  x$setinverse(inv)
## report the inverse to the user using the variable inv
  inv
}


