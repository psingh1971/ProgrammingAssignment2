
############################################################################################################
## The two functions below allow for a matrix to cache its inverse and access it from the cache ############
## They take advantage of the scoping rules of R to store the inverse of a matrix with the original matrix #
## This approach can save processing times for large matrices requiring frequent inversions ################
############################################################################################################


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse ###############
## It returns a list of 4 functions that can be used to set values to the matrix and its inverse ###########
## as well as to retrieve them #############################################################################

## Here the parent environment for the functions: get, set, getinverse, and setinverse is that of the ######
## function makeCacheMatrix. This environment is saved with the "special matrix" object that is ############
## created when makeCacheMatrix is called. Everytime these functions are called as elements of the list ####
## they interact with this enclosed environment, e.g. to set value to this object, retrieve it, etc.########



makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()         
        set <- function(y) {
                x <<- y           ## Assign the value in the argument to x in the parent environment
                inv <<- matrix()  ## Reset the inverse of the matrix in the parent environment
        }
        get <- function() x   
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        list(set = set, get = get,               ## Creates a list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above #
## If the inverse has already been calculated, and the matrix has not changed; then it retrieves ##########
## the inverse from the cache #############################################################################


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(all(!is.na(inv))) {                   ## Returns the cached inverse matrix if it exists
                message("getting cached data") 
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


#################################################################################################
#### Tests to verify that the functions work as intended ########################################
#################################################################################################

#m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))   # Creates a special "matrix" object

#m$get()                # Returns original matrix
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#cacheSolve(m)         # Computes, caches, and returns the matrix inverse
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#m$getinverse()        # Returns matrix inverse directly using the function within the matrix
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#cacheSolve(m)         # Returns cached matrix inverse using previously computed matrix inverse
#getting cached data
#       [,1] [,2]               
#[1,]   -2  1.5
#[2,]    1 -0.5


#m$set(matrix(c(0,5,99,66), nrow=2, ncol=2))    # Modifies existing matrix

#cacheSolve(m)           # Computes, caches, and returns new matrix inverse
#       [,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0

#m$get()                 # Returns the modified matrix
#       [,1] [,2]
#[1,]    0   99
#[2,]    5   66

#m$getinverse()          # Returns matrix inverse
#       [,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0

