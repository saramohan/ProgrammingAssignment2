
## The function setCacheMatrix takes a matrix as a argument.
## It returns a list of the various functions that can be
## used to store the inverse of the matrix in cache.

## get() function returns the matrix for which the inverse is to be calculated.
## set() function creates a new matrix with inverse NULL.
## getinverse() function returns the inverse of the matrix in cache.
## setinverse() function is used to store the inverse of a matrix in cache.


makeCacheMatrix <- function(x = matrix())
{
  inverse_result<-NULL
  set_matrix <- function(new_matrix)
  {
    x <<- new_matrix
    inverse_result <<- NULL
  }
  get_matrix <- function() 
  { 
    x 
  }
  set_inverse <- function(inv_m)
  {
    inverse_result <<- inv_m
  }
  get_inverse <- function()
  { 
    inverse_result
  }
  list(set = set_matrix,get = get_matrix,setinverse = set_inverse,getinverse = get_inverse)	
}


## The cacheSolve Function takes in the special matrix returned
## from the makeCacheMatrix function.
## It checks if the inverse of the patricular matrix is present in cache.
## If present it returs the value of the inverse.
## Else the inverse is calculated and stored in cache.

cacheSolve <- function(x, ...) 
{
  inver_result<-x$getinverse()
  tmp_inv<-solve(x$get())
  if(!is.null(inver_result) && (nrow(inver_result) == nrow(tmp_inv)) && (ncol(inver_result) == ncol(tmp_inv)) )
  {
    if (identical(inver_result,tmp_inv))
    {
      message("Getting from Cache")
      return(inver_result)
    }
  }
  inver_result <- tmp_inv
  x$setinverse(inver_result)
  message("Not present in Cache!! Solving ....")
  inver_result
}

