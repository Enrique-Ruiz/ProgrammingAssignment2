## The functions in this file are used to create a special object that stores 
## a numeric vector and cache's its inverse.

## NOTE: For these functions I borrowed heavily from the example functions given
## in the assignment instructions.

## The first function, creates a special "vector", which is actually a list 
## containing a function that...
##      sets the value of the vector
##      gets the value of the vector
##      sets the value of the inverse
##      gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function calculates the inverse of the special "vector" created with 
##  the above function. But it first checks to see if the inverse has already 
##  been calculated.  If so it gets the inverse from the cache.  If not it 
##  calculates the inverse of the data and sets the value of the inverse in the 
##  cache via the setmean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
