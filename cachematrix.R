## Basically I followed the same algorithm of the mean's vector. so the function make the following:
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inv
## 4 get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
inv=null
set=function(y){
x=y
inv=null
}
get = function() x
setinv = function(inverse) inv <<- inverse 
getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inv from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv
##function

cacheSolve <- function(x, ...) {
 inv = x$getinv()
        
      if (!is.null(inv)){
 
        message("getting cached data")
        return(inv)
        }
        

        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
