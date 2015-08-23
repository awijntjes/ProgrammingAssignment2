## These functions cache the inverse of a matrix


## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL  
set_inv<-function(y)
{ 
x<<-y 
m<<-NULL 
}
get_inv<-function() 
x
setmat<-function(solve) 
m<<- solve
getmat<-function() 
m
list(
setmat=setmat,
getmat=getmat, 
set_inv=set_inv,
get_inv=get_inv,)
}
}

## cacheSolve computes the inverse of 
## the special "matrix" returned by  makeCacheMatrix  above

cacheSolve <- function(x, ...) 
{
m<-x$getmat()
if(!is.null(m))
{
message("Getting Cached Data")
return(m)
}
matrix<-x$get_inv()
m<-solve(matrix, ...)
x$setmat(m)
m