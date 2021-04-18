
## 2 type functions: makeCacheMatrix, cacheSolve
## makeCacheMatrix: set, setinv, get, getinv in it

makeCacheMatrix <- function(x = matrix()){
  inv<-NULL #to initialize inverse as NULL term
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)
    getinverse<-function(){inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##used to get the cache data needed

cacheSolve <- function(x, ...) ## cache data
{}
inv<-x$getinverse()
if(!is.null(inv)){ ##to verify null output inverse
  message("getting the value of cache data")
  return(inv) 
}
value<- x$get()
x$setinv(inv)
inv ##returns a value matrix that is the inverse of the value 'x'
