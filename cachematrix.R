## cachematrix.R
## 
##  Overall description of the cachematrix.R functions:
##
##  Caching the Inverse of a Matrix
##
##  Matrix inversion is usually a costly computation in R.  Functions for
##  caching the inverse of a matrix, rather than computing it repeatedly, 
##  attempt to reduce the cost of matrix computation. 
##
##  Caching the Inverse of a Matrix involves a pair of functions that cache a 
##  matrix and the inverse of the matrix:
##    1.  makeCacheMatrix <- function(x = matrix()) 
##    2.  cacheSolve <- function(x, ...)
##
##  Functions assume that the matrix supplied is always invertible.
##
##  Usage, eg:
##    > source("cachematrix.R")
##    > x <- matrix(5:8, nrow = 2, ncol = 2)
##    > a <- makeCacheMatrix(x)
##    > a$set(x)		# set
##    > a$get()			# get
##         [,1] [,2]
##    [1,]    5    7
##    [2,]    6    8
##    > a$getinverse()		# getinverse: returns NULL b/c inverse not set
##    NULL			#  yet by cacheSolve or setinverse
##    > cacheSolve(a)		#  a is list created by: a <- makeCacheMatrix(x)
##         [,1] [,2]		# cacheSolve 1st call computes matrix inverse,
##    [1,]   -4  3.5 		#  calls setinverse to set, & returns inverse
##    [2,]    3 -2.5
##    > cacheSolve(a)		# cacheSolve 2nd call returns cached data
##    getting cached inverted matrix data	 # msg re: getting cached data
##         [,1] [,2]
##    [1,]   -4  3.5
##    [2,]    3 -2.5
##    >                         
##    > a$getinverse()		# getinverse: now returns cached inverse
##         [,1] [,2]		# after being set using cacheSolve
##    [1,]   -4  3.5
##    [2,]    3 -2.5
##    > 
##    # verify that when the matrix is reset we do NOT return previous inverted
##    # matrix from cache:
##    >  x <- matrix(1:4, nrow = 2, ncol = 2)
##    > a$set(x)
##    > a$get()
##         [,1] [,2]
##    [1,]    1    3
##    [2,]    2    4
##    > a$getinverse()
##    NULL
##    > cacheSolve(a)
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    > cacheSolve(a)
##    getting cached inverted matrix data
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    >
##				
##
##  Author: J Go, R Programming Course, Programming Assignment 2,
##          Data Science Specialization, Johns Hopkins University, 
##          Bloomberg School of Public Health, on Coursera, September 2015.  
##          Instructors: Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD.
##
##  Date last updated: 09/27/15
## 


makeCacheMatrix <- function(x = matrix()) {
	## Function description:
	##  Create a special "matrix" object that can cache its inverse.
	##  
	##  The "matrix" object is a list of functions for computing on the 
	##  matrix.  This list contains functions to:
	##  
 	##  1. set the value of the stored matrix to a new value
 	##  2. get the value of the stored matrix
 	##  3. set the inverse of the matrix to an argument
	##  4. get the inverse of the matrix that was stored
	## 
	##  makeCacheMatrix is a function closure.
	## 

        ## Error processing
	
	if (!is.matrix(x)) {
		stop("'x' must be a square matrix.  Please fix and retry.")
	}
        
        inverse <- NULL				# initialize inverse to NULL
	
        set <- function(y) {			# set cached matrix
                x <<- y
        	inverse <<- NULL		# set cached inverse to NULL 	
        }					#  after set matrix

        get <- function() x			# get matrix

						# called by cacheSolve to
						# set cached inverse:
        setinverse <- function(inverse) inverse <<- inverse

        getinverse <- function() inverse	# get currently stored inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
	## Function description:
        ##  Return a matrix that is the inverse of 'x'.
	##  Compute the inverse of the special "matrix" returned by the 
	##  makeCacheMatrix function.  
	## 
	##  If the inverse has already been calculated (and the matrix has not 
	##  changed), then the cacheSolve should retrieves the inverse from the 
	##  cache.
	## 
	## How inverse is computed:
	##  If X is a square invertible matrix, solve(X) returns its inverse.
        ##
	##  cacheSolve is a function closure.
	## 
        ##  Usage and Arg:
	##   > cacheSolve(y)	# note, y is a list object created as:
	##			#       y <- makeCacheMatrix(x)
        ##			# where x is a square invertible matrix
        ##  Returns:
        ##    The inverse of square matrix x supplied when creating arg y
	##    as shown in Usage and Arg above. 

         inverse_x <- x$getinverse()
         if(!is.null(inverse_x)) {
                 message("getting cached inverted matrix data")
                 return(inverse_x)
         }
         data <- x$get()
         inverse_x <- solve(data, ...)		# solve(x)
         x$setinverse(inverse_x)
         inverse_x
}
