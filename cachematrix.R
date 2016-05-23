## Functions below compute and cache the inverse of a matrix.
## makeCacheMatrix returns a "matrix" object (list containing the functions: 
## set the matrix, get the matrix, set the inverse, get the inverse) that are 
## used as input to cacheSolve(). use set the matrix function to input original matrix.
## cacheSolve() returns the inverse of the original matrix input to makeCacheMatrix()


## creates a special matrix object that can cache its inverse.
## return: list containing functions:
## 1. set matrix
## 2. get matrix
## 3. set inverse
## 4. get inverse
## this is list is used as input for cacheSolve()
## use set the matrix function to input original matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculates the inverse of the matrix returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it obtains the inverse from the cache directly.
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


