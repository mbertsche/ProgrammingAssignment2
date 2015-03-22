## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
      inverse <- NULL
      
      set <- function(z){
            mat <<- z
            inverse <<- NULL
      }
      
      get <- function(){
            mat
      }
      
      
      setInverse <- function(inv){
            inverse <<- inv
      }
      
      getInverse <- function(){
            inverse
      }
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      
      if(!is.null(inverse)){
            message("getting cached matrix")
            return(inverse)
      }
      
      mat <- x$get()
      inverse <- solve(mat)
      x$setInverse(inverse)
      inverse
}
