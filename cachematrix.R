## Put comments here that give an overall description of what your
## functions do

## Author: Dave Belding
## Matrix inversion caching functions
## These functions are to demonstrate the use of cached objects.  In
## this case, a matrix will be inverted only if the inverse has not 
## already been created and cached.

##Usage Example:
##
##mtx <- matrix(c(1,5,5,1), nrow=2, ncol=2) #create a 2x2 invertible matrix
##mtxCache <- makeCacheMatrix() #create a cache matrix
##mtxCache$set(mtx) #set cache matrix to 2x2 matrix
##cacheSolve(mtxCache) #solves matrix first time without using cache
##cacheSolve(mtxCache) #says "Getting cached data..."
##mx$getinverse() #returns inverted matrix

## Notes from Course:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y = NULL) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##changed), then cacheSolve should retrieve the inverse from the cache.
##It is assumed for this project that the given matrix is solvable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)){
				message("Getting cached data...")
				return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i
}


##
##Write the following functions:
##
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if 
##X is a square invertible matrix, then solve(X) returns its inverse.
##
##For this assignment, assume that the matrix supplied is always invertible.