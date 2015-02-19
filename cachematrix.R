## These functions (makeCacheMatrix & cacheSolve) 
## calculate the inverse of a matrix
## using the solve() function

## makeCacheMatrix takes a matrix saved in the variable x
## and returns a list of functions 
## which are called by the cacheSolve function.
## The result of the matrix inversion is cached in the containing environment
## using the super assigment operator (<<-)
## so for the same matrix it can be re-used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)  
}

## This function checks if the inverse has already been cached
## if it has then it returns the cached value, 
## otherwise it performs the inversion using solve()
## and returns the result

cacheSolve <- function(x, ...) {
       
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
