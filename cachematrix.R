#coursera R programming language course, week 3

# make cache matrix function
# creates object that can store matrix and its inverse
makeCacheMatrix <- function(x = matrix())
{
  #whenever data gets changed, set inverze to null
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  #get data from object (original matrx)
  get <- function()
  {
    x
  }
  
  #set inverse to inverse of x
  setinverse <- function(inverse)
  {
    inv <<- inverse
  }
  
  #get inverse, if inv is not defined return null from one scope up
  inv <- NULL
  getinverse <- function()
  {
    inv
  }
  
  #set all values to object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#cache solve function, ... are passed to solve function that calculates inverse
#reads inverse if exists otherwise calculates it and stores it in object
cacheSolve <- function(x, ...)
{
  #if inverse is already calculated dislay message and return it
  if(!is.null(x$getinverse()))
  {
    message("reading from memory")
    return(x$getinverse())
  }
  #else calculate inverse
  inv <- solve(x$get(), ...)
  #store it in object
  x$setinverse(inv)
  #and return it
  inv
}
