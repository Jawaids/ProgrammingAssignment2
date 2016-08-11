## Put comments here that give an overall description of what your
## functions do

## The two functions shown here help in caching the inverse of a matrix.
## Matrix inversion is usually very computationally intensive
## especially for large size matrices.



## Write a short comment describing this function
## Function “makeCacheMatrix” creates a special “matrix” object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    flag <- NULL
    set <- function( y ) {
        x <<- y
        flag <<- NULL
    }
    get <- function() x
    setInverse <- function( iMatrix ) flag <<- iMatrix
    getInverse <- function() flag
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}



## Write a short comment describing this function
## Function “cacheSolve” computes the inverse of the special “matrix”
## (which is the input of cachemean) returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null( m ) ) {
        message( "Now using Cached Data." )
        return( m )
    }
    data <- x$get()
    m <- solve( data, ... )
    x$setInverse( m )
    m
    ## Return a matrix that is the inverse of 'x'
}
