## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following function "makeCacheMatrix" creates a special "matrix", which
## is a list containing a function to:
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(i) m <<- solve(x)
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

## The following function "cacheSolve" calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if the inverse of the matrix
## has already been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the inverse
## matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m))
        {
                return(m)
        }
        m <- solve(x$get())
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
