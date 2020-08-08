makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y){
    if(is.null(y)){
      message("Null value entered")
      return()
    }
    x<<-y
    inv<<-NULL
  }
  get<- function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function(y, ...) {
  x<-makeCacheMatrix(y)
  inv<- x$getinverse()
  if(!is.null(inv))
  {
    message("getting from cache")
    return(inv)
  }
  data<-x$get()
  if(!isSquare(data))message("Matrix is not square")
  
  else{
    inv<-solve(data)
    x$setinverse(inv);
    message("Set cache data")
    inv
  }
}
isSquare<- function(x=matrix())
{
  return (nrow(x)==ncol(x)) && det(x)!=0
}
