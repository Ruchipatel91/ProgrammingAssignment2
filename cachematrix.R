#Ruchi Patel 5/30/2018
#Programming assignment 2(PEER GRADED)
#Inverse the matrix by caching

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) { #SET THE FUNCTION
    x <<- y
    i <<- NULL #INITIATES THE VECTOR
  }
  
  get <- function() x #GET THE FUNCTION
  
  set_inverse <- function(inverse) i<<- inverse
  
  get_inverse <- function() i
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
  
  
}


cacheSolve <- function(x, ...) {
    
  
  i <- x$get_inverse() #GET THE PREVIOUS VALUE OF CACHED MATRIX
  
  if(!is.null(i)) { # checks the condition for i... if it has chached value then it will print msg.
    message("Getting cached MATRIX")
    return(i)
  }
 
  m<- x$get()#GET THE MATRIX PASSED TO THE FUNCTION
  
  i <- solve(m, ...) #GIVES INVERTED MATERIX
  
  x$set_inverse(i) #SET VALUE OF INVERTED MATRIX
  
  i #RETURN INVERTED MATRIX
  
  
}



