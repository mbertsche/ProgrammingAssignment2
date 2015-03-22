## This pair of functions takes a square matrix, solves the inverse of it and
## caches that value so that solve() does not need to run each time the value
## is needed

## This function creates the cachable matrix and inverse by using <<- to set 
## the values to the parent environment.  It provides individual function to set
## the matrix, get the matrix, get the inverse and set the inverse

makeCacheMatrix <- function(mat = matrix()) {
      inverse <- NULL
      
      ## takes the matrix as an arguement, sets the matrix to parent environment
      ## varible mat. Initializes the inverse
      set <- function(z){
            mat <<- z
            inverse <<- NULL
      }
      
      ## returns the matrix stored in the parent environment
      get <- function(){
            mat
      }
      
      ## takes the inverse as an arguement and stores it in the parent environment
      ## varible inverse
      setInverse <- function(inv){
            inverse <<- inv
      }
      
      ## retrieves and returns the inverse stored in the parent environment 
      ## varible inverse
      getInverse <- function(){
            inverse
      }
      
      ## the list of functions that an object created with makeCacheMatrix() 
      ## can call
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse.  It uses the getInverse() function to get the inverse 
## of the matrix.  If the inverse is not NULL, the inverse will be returned 
## without needing to run solve().  If inverse is null, then it gets the matrix
## and solves it.  It then sets the inverse and returns the it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      ## retrieves the inverse of the parent environment variable inverse and 
      ## stores it in the local variable inverse
      inverse <- x$getInverse()
      
      ## checks to see if the inverse is not NULL.  If it is not, then returns
      ## the inverse
      if(!is.null(inverse)){
            message("getting cached matrix")
            return(inverse)
      }
      
      ## gets the matrix and stores it locally.  Solves the matrix and uses it
      ## to set the inverse of the parent environment variable via $setInverse()
      mat <- x$get()
      inverse <- solve(mat)
      x$setInverse(inverse)
      inverse
}
