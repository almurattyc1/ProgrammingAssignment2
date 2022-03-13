## This function is designed to create a special matrix that 
## is really a list. Once the "empty" matrix is created it is populated with
## the input matrix. Finally, the inverse of the input matrix in computed

## The makeCacheMatrix() when called, first empties the "inv" matrix and then creates 
##a special matrix that is in list form so that cachesolve() can compute the inverse using
## solve ()

makeCacheMatrix <- function(z = matrix()) {
  inv <<- NULL  #empty the inv matrix
  get <- function() z #list to hold the original matrix - "z"
  setinv <- function(inverse) inv <<- inverse # list to hold the inverse matrix from cachesolve
  getinv <- function() inv #another list to hold an inverse matrix from a previous computation
  list(get = get, setinv = setinv, getinv = getinv) #creation of the special matrix list used by cachesolve
}


## cacheSolve() will first check if an inverse matrix exists in getinv list that 
## alignes with the z input
## If it does it will use the cached getinv, if it does not exist it will
## compute the inverse of the matrix "z" and append it to the setinv list

cacheSolve <- function(z, ...) {
  inv <- z$getinv() #get the inverse matrix in special list
  if(!is.null(inv)){ # check to see if there is data 
    message("getting cached data") #display message is inv is already full
    return(inv) #show cached "inv
  }
  mat.data <- z$get()  # If getinv is not populated, get "new" input matrix
  inv <- solve(mat.data, ...) #compute inverse of the input matrix
  z$setinv(inv) #append setinv list with new inverse
  inv #display inverse of matrix
}