## Put comments here that give an overall description of what your
## functions do:


## The functions below set a matrix and create functions which are 
## subsequently used in another function to return the inverse
## of the matrix 



## Write a short comment describing this function:

## This function sets a matrix and creates a list of functions 
## in a global environment.  The functions are used in the  
## function cacheSolve() 

makeCacheMatrix <- function(x = matrix()) {
    ## x must be a square invertable matrix
    m <- NULL
    # function to set the matrix
    set <- function(y) {
        ##  "super assignment" to assign in the global environment
        x <<- y
        m <<- NULL
    }
    # function to get the matrix
    get <- function() x
    # function to set the solution... cache for later
    setsolve <- function(solve) m <<- solve
    # get the solution... if one already exists
    getsolve <- function() m
    # construct the list of functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## Write a short comment describing this function:

## function cacheSolve returns a matrix that is the inverse of x
## if available, the cached answer will be returned saving processing time.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        # if m is not null then it doesn't have to run the 
        # calculation again and can use the cached answer
        message("getting cached data")
        return(m)
    }
    # if m is null then it gets the data and caluclate the solution
    data <- x$get()
    m <- solve(data, ...)
    # then sets the solution in the cache for later if needed
    x$setsolve(m)
    # then returns the result to view...
    m
}

## In order to test my functions I also created the following function 
## to generate a matrix to use ... I had a hard time wrapping my head  
## around square invertable matrices and inverse matrices so this
## helped me...

mymat <- function(x){
    # x is the number of elements in the matrix
    # generate random
    y=rnorm(x)
    # the matrix must be square so use
    # the square root of the elements
    n=sqrt(x)
    # create my matrix
    m =matrix(y, nrow=n, ncol=n)
    m
}

