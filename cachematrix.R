## Put comments here that give an overall description of what your
## functions do
        ## Return a matrix that is the inverse of 'x'
##
##Following makeVector sample code provided in Programming Assignment 2 instructions
##but using makeCacheMatrix instead of makeVector function.
makeCacheMatrix <- function(x = matrix()) ## Creates a special vector "matrix" that can cache its inverse
{
  m <- NULL
  set<-function(y) ## set the value of the vector
    { 
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setmatrix  <- function(solve()) m <<- solve() ## set the value of the matrix
  ## that can cache its inverse using the solve function once stored
  getmatrix <- function() m ## get the value of the matrix
  list(setmatrix = setmatrix, getmatrix=getmatrix,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cacheSolve <- function(x=matrix(), ...) ## # This function gets the inverse of the special "matrix"
  ##from  makeCacheMatrix function above
{
  m <- x$getmatrix()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get
  m <- cache(matrix, ...)
  x$setmatrix(m)
  m
}