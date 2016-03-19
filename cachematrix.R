## cachematrix() contains convenience functions for 
## matrix storage and inversion. The matirx 'x' will have 
## its inverse calculated once. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## the cachesolve() should retrieve the inverse from the cache.

## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    
    set <- function(y){
        x <- y
        im <<- NULL
    }
   
    get <- function() x
    setmatrix <- function(solve) im <<- solve
    getmatrix <- function() im
    
    list( set=set, get=get, 
          setmatrix=setmatrix,
          getmatrix=getmatrix)
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    ## This function assume that the matrix supplied is 
    ## always invertible.
    
    im <- x$getmatrix()
    if(!is.null(im)){
        message("Getting cached data..")
        return (im)
    }
    m <- x$get() 
    im <- solve(m, ...)
    x$setmatrix(im)
    
    ## Return a matrix that is the inverse of 'x'
    im
}
