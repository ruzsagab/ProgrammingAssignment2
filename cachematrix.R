## The function <makeCacheMatrix> creates an object with four attributes: <set>, <get>, <setInv>, <getInv>;
## <set> is a function to accept a matrix input and store it;
## <setInv> is a function to accept a matrix inverse input and store it;
## <get> is a function to retrieve a matrix stored in the <set> attribute of the object;
## <getInv> is a function to retrieve a matrix inverse stored in the <setInv> attribute of the object.
makeCacheMatrix <- function(matr = matrix()) {
  ## Accept <matr> as a valid argument only if it is a square matrix; otherwise send a warning.
  if (class(matr)=="matrix") {
    if (dim(matr)[1]==dim(matr)[2]) {
    } else {
      matr<-matrix()
      warning("invalid data input")
    }
  } else {
    matr<-matrix()
    warning("invalid data input")
  }
  
  inv <- NULL
  
  fnSet <- function(y) {   # function to accept a matrix input and store it in <matr>
    ## Accept <y> as a valid argument only if it is a square matrix; otherwise send a warning.
    if (class(y)=="matrix") {
      if (dim(y)[1]==dim(y)[2]) {
        matr <<- y
        inv <<- NULL
      } else warning("invalid data input")
    } else warning("invalid data input")
    invisible(y)    
  }
  fnGet <- function() matr   # function to retrieve a matrix stored in <matr>
  fnSetInv <- function(y) inv <<- y   # function to accept a matrix inverse input and store it in <inv>
  fnGetInv <- function() inv   # function to retrieve a matrix inverse stored in <inv>
  
  ## The return value to <makeCacheMatrix> is a list of <set>, <get>, <setInv>, <getInv> attributes.
  list(set = fnSet, get = fnGet, setInv = fnSetInv, getInv = fnGetInv)
}


## The function <cacheSolve> accepts as its argument an object <matr> created with the <makeCacheMatrix> function;
## if <matr$getInv> contains a matrix, it is returned as the cached matrix inverse of <matr$get>;
## if <matr$getInv> is empty, the inverse of <matr$get> is computed and cached in <matr$getInv>.
cacheSolve <- function(matr) {
  inv <- matr$getInv()   # retrieve the matrix inverse cached in <matr$getInv>
  
  ## If the cached matrix inverse is non-empty, return it as the <cacheSolve> function value.
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  ## If the cached matrix inverse is empty, calculate and cache the inverse of the matrix contained in <matr$get>.
  currMatr <- matr$get()
  inv <- solve(currMatr)
  matr$setInv(inv)
  inv
}