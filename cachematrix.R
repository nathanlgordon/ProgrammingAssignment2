## R Programming Assignment 2

## the makeCacheMatrix essentially creates a "matrix" by setting the value of the matrix, 
##then getting that same value
##while at the same time setting the value for the inverse  (using function(inverse))
## and getting that inverse 

makeCacheMatrix <- function(x = matrix()) {
  z<-NULL
  set<-function(y){
    x<<-y
    z<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) z<<- inverse
  getinverse<-function() z
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve calculates the inverse of the matrix using if it has not yet been calculated (and is sitting in cache)


cacheSolve <- function(x, ...) {
  z<-x$getinverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  data <-x$get()
  z<-solve(data, ...)
  x$setinverse(z)
  z
}
