## Cache a matrix's inverse
## Author:Ganqi Ou, Date:2014-05-20
## Just Note: matrix must be a square matrics and |A| not equals zero

##construct a special Matrix which contains its inverse

makeCacheMatrix <- function(x = matrix()) {
      ## The <<- operator create eviroments
  ins <- NULL
  
  set <- function(y){
    x <<- y
    ins <<- NULL
  }
  get <- function() x
  
  setinverse <-  function(inverse) ins <<- inverse
  getinverse <- function() ins
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## solve a Matrix's inverse,if it has computed,get the cache,else compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x',also give a prompt message
  ins <- x$getinverse()
  
  if(!is.null(ins)){
    message("getting cached data")
    return(ins)
  }
  
  data <- x$get()
  ins <- solve(data)
  x$setinverse(ins)
  
  ins
}
