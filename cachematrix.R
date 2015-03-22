## Put comments here that give an overall description of what your
## makeCacheMatrix function creates the special matrix object with cached inverse value
## cacheSolve function counts the inverse of special matrix object, if the inverse was already computed it only return the value

## Write a short comment describing this function
## functions do initialization of special matrix object
## get function returns the value of object
## set function assigns the value to object
## getinv returns the inverse of square matrix computed with solve function
## setinv assigns the inverse matrix value to object

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m <<-solve
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## Write a short comment describing this function
## function compute the inverse of matrix
## at first the function checks if inverse if already computed, if so, the value is immediately returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting inverse from cache")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}
