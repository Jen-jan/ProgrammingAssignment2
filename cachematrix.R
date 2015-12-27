## A pair of functions to cache the inverse of a matrix to save computation time


makeCacheMatrix <- function(x = matrix()) {
        ## Create a special "matrix" object from a matrix x, that can cache its
        ## inverse.
        ##
        ## Args:
        ## x: an invertible matrix
        ##
        ## Returns:
        ## a list of functions
        
        m <- NULL
        set <- function(y) {  ## set the vaue of the matrix x outside current environment
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## returns value of matrix x
        setinverse <- function(solve) m <<- solve  ## sets m outside current environment
        getinverse <- function() m   ## returns m
        list(set = set, get = get,   ## create special object that containns cache
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## returns the cached invertible matrix if it already exists, or solves
        ## for the inverse otherwise
        ##
        ## Args:
        ## x: the result of the makeCacheMatrix function
        ##
        ## Returns:
        ## the inverse of the matrix input into the makeCacheMatrix function
        
        m <- x$getinverse() 
        if(!is.null(m)) {   ## check that m exists
                message("getting cached data")
                return(m)    ## if m exists return m and end function
        }
        data <- x$get()   ## if m does not exist solve the inverse and return
        m <- solve(data, ...)
        x$setinverse(m)
        m
}