## the makeCacheMatrix function creates a "special vector" containing a list of functions.
## 2 functions to get the values of stored things. (the value of the original matrix and the value of the inverse matrix)
## 2 functions to set the values of things to store. (the value of the original matrix and the value of the inverse matrix) )

makeCacheMatrix <- function(x = matrix()) { 
        im <- NULL              ## Sets the value of the inverse matrix variable to empty, in case the cachesolve function is not used yet
        setm <- function(y) {   ## set the value of the matrix
                x <<- y         ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
                im <<- NULL     ## sets the value of im (the inverse matrix in case the cacheSolve is used) to NULL
        }
        getm <- function() x    ## this function returns the value of the original matrix
        setinverse <- function(solve) im <<- solve      ## this is called by cachesolve() during the first cachesolve() access and it will store the value using superassignment
        getinverse <- function() im                     ## this will return the cached value to cachesolve() on subsequent accesses
        list(setm = setm, getm = getm, ## creates a list with the four functions
             setinverse = setinverse, 
             getinverse = getinverse)
}

## the cachesolve function calculates the inverse matrix of an matrix, 
## but first check's if the inverse is already calculated. 
## It uses the "special vector'created by the makeCacheMatrix function

cacheSolve <- function(x, ...) {        # the input x is an object created by makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()    ## gets the value of the inverse matrix from the object 'x'
        if(!is.null(im)) {      ## if inverse is already present
                message("getting cached data")  ## print the message: "getting cached data" 
                return(im)                      ## return the inverse and ends the function 
        }
        data <- x$getm()        ## we reach this code only if x$getinverse() returned NULL
        im <- solve(data)  ## if im was NULL then we have to calculate the inverse
        x$setinverse(im)        ## store the calculated inverse value in x (see setinverse() in makeVector
        im                      ## return the mean to the code that called this function
}
