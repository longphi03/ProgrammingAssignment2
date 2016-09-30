## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix function will create a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## Return set/get the special matrix obj with the input matrix 
  ## and its inverse
  
  ## check if x is invertible 
  ##result <- try(determinant(x)$modulus)
  inv = NULL
  if (class(try(solve(x), silent = T)) != "try-error"){
    #print("i'm here!")
    set = function(y){
      x <<-y
      inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }else{
    print("x not an invertible matrix")
  }
}


## cachesolve function take in the return value of make cachmatrix
## If the inverse has already been calculated and the matrix has 
## not changed, it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  #get x's invert from cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }else{# calculate x's invert
    mat = x$get()
    inv = solve(mat, ...)
    x$setinv(inv)
    return(inv)
  }
}
