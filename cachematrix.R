## Put comments here that give an overall description of what your
## functions do

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invrs<-NULL ## initializing the variable with null
  set<-function(y)  ## set function which sets the value of x and null to invrs
  {
    x<<-y
    invrs<<-NULL
  }
  get<-function(){x} ## gets the variable of x
  setInverse<-function(inverse)
  {
    invrs<<-inverse    ## sets the invrs variable value
  }
  getInverse<- function() ##gets the invrs value
  {
    invrs
  }
  list (set=set , get = get ,setInverse=setInverse ,getInverse=getInverse) ## list making of all the variables according to their name
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs<- x$getInverse() ##assign previous cache value if any
  if(!is.null(invrs))
  {
    message("getting cached data") ## check if there is any cache exist
    return(invrs)
  }
  
  mat<-x$get() ## assign the value of x to mat using get function previously set
  invrs<-solve(mat,.....)
  x$setInverse(invrs)
}
