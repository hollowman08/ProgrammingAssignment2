## Caching Inverse of a matrix 

## Creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) { 
  m<-NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m <<-matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
} 




## Function computes the inverse of the special matrix returned by makeCacheMatrix


cacheSolve <- function(x=matrix(), ...) { 
## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)) {
    message("getting cashed data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setmatrix(m)
  m
} 
