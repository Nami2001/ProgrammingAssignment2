## Put comments here that give an overall description of what your
## functions do

## Bascially there consist two kinds of functions: makeCacheMatrix, cacheSolve
## makeCacheMatrix has set, setinv, get, getinv in it
## library(Mass) has been used to get inverse for both non squared and squared matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
inv<-NULL #to initialize inverse as NULL term
set<- function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setinv<-function(inverse)inv<<-inverse
getinv<-function(){
                  inver<-ginv(x)
                   inver%*%x #this is used as a function to get the inverse of the matrix needed
                    }
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## Write a short comment describing this function
##This very function is used to obtain the cache data needed

cacheSolve <- function(x, ...) ##this gets the cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){ ##to verify if the inverse is a null output
    message("obtaining cache data")
    return(inv) ##returns the inverse value
  }
  data<- x$get()
  inv<-solve(data,...) ##computes for the inverse value needed
  x$setinv(inv)
  inv ##returns a value matrix that is the inverse of the value 'x'
}

