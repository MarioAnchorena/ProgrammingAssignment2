## Put comments here that give an overall description of what your
## functions do

## makes matrix and returns list of functions that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {

    invM=NULL #Initializes matrix inverse as NULL
    
    # Set the matrix values in the makeCacheMatrix environment
    set=function(y){ 
        x<<-y
        invM<<-NULL
    }
    
    # Get the matrix values cached in the makeCacheMatrix environment
    get=function() x 
    
    # Set the inverse matrix values in the makeCacheMatrix environment
    setInvM=function(inverse) invM<<-inverse
    
    # Get the inverse matrix values cached in the makeCacheMatrix environment
    getInvM=function() invM
    
    # Return list of functions
    list(set=set,get=get,
         setInvM=setInvM,
         getInvM=getInvM)
}


## Gets inverse of matrix from cache or computes inverse of matrix

cacheSolve <- function(x, ...) {
    
    invM <- x$getInvM() # Gets inverse of matrix if its cached in  makeCacheMatrix environment
    
    # Returns inverse of matrix if it is cached in makeCacheMatrix environment
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    
    data <- x$get() # Gets matrix cached in  makeCacheMatrix environment
    
    invM <- solve(data, ...) # Computes inverse of matrix
    
    x$setInvM(invM) # Sets inverse of matrix to the makeCacheMatrix environment
    
    invM # Return a matrix that is the inverse of 'x'    
}
