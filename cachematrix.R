## Put comments here that give an overall description of what your
## functions do

## This function creates a special object that contains a list of
## functions and values to be called by cacheSolve to compute a 
## matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL #assign the matrix inverse to null within this function
    
    get <- function() x #x is the input to makeCacheMatrix
    
    #set the inverse, mi, globally using the "<<-" operator
    setinverse <- function(inverse) mi <<- inverse
    
    getinverse <- function() mi #return the value of mi
    
    #store all functions in a list
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes the output of makeCahceMatrix and first 
## checks to see if the matrix inverse exists, if it does not, it 
## computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    mi <- x$getinverse()
    if(!is.null(mi)){ #mi exists, return the inverse
        message("getting cached data")
        return(mi)
    }
    
    # if the inverse exists, the function will exit on "return
    # compute the inverse, and store it with the "setinverse" function
    
    mi <- solve(x$get())
    x$setinverse(mi)
    mi
}
