## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    #set a new matrix
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    #get the current matrix
    get <- function() x
    
    #set inverse of matrix
    setInverse <- function(inv) inverse <<- inv
    
    #get the inverse of matrix
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    #verify if the inverse of matrix has been calculated
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse) #return the inverse
    }
    
    #If the inverse has not been calculated then get the matrix
    #calculated the inverse, set the inverse and return the inverse
    data <- x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}

