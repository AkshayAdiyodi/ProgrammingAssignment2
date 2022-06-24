## Functions that cache the inverse of a matrix
## Function to get & set both matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<- function()x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Function to inverse the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i

}
