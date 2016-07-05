## Function makeCacheMatrix
## Description : This function creates a list of matrixes with their inverses 

## This function creates a cache-wrapper around the computation of the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  # initialize inv_mat
  inv_mat <- NULL
  # function setting x and inv outside calling environment
  set_mat <- function(y){
      x <<- y
      inv_mat <<- NULL
  }
  # function returning the original matrix
  get_mat <- function() x
  # function setting local variable with 
  set_mat_inv <- function(inv) inv_mat <<- inv
  # function returning inv_mat
  get_mat_inv <- function() inv_mat
  # returning the list
  list(set_mat = set_mat, get_mat = get_mat, set_mat_inv = set_mat_inv, 
         get_mat_inv = get_mat_inv)
    
  
}


## This function tries to lookup an already calculated inverted matrix
## If the matrix is not present in the list it will compute the inverse and store it in the cache.
## the input is the output of the makeCacheMatrix funtion
cacheSolve <- function(x, ...) {
  
  inv <- x$get_mat_inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get_mat()
  # computing the inverse matrix
  message("computing the inverse of a matrix")
  inv <- solve(data)
  # store inverse matrix in the cache 
  x$set_mat_inv(inv)
  inv
}


## 
## To Test

# test <- makeCacheMatrix()

#test$set_mat(matrix( 
#c(2, 4, 3, 2,3,4,1, 5, 7), 
#nrow=3, 
#ncol=3))

#cacheSolve(test)
## expect message of computing a matrix is displayed

#cacheSolve(test)
## expect message of getting  is displayed

#test$set_mat(matrix( 
#c(2, 4, 3, 2,3,4,1, 5, 2), 
#nrow=3, 
#ncol=3))

#test$get_mat_inv
## expect null
