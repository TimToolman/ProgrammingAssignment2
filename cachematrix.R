## The following functions, "makeCacheMatrix" and "cacheSolve" create a matrix that is then used
## via cache to retrieve the inverse of the initially created matrix.

## First function creates a matrix - "my_matrix"
        makeCacheMatrix <- function(x = matrix()) {

                my_matrix <- NULL                                       # Set my_matrix to NULL to clear all data
                set <- function(y) {                                    # Function used to set the value of the matrix
                        x <<- y                                         # <<- Operator used in functions
                        my_matrix <<- NULL                              # If the matrix is reset, then we need to delete the previous computed inverse
                }
                
                get <- function() x                                     # Get original matrix
                setInverse <- function(my_matrix_inv) my_matrix <<- my_matrix_inv   # Function used to set the inverse, once we know it
                getInverse <- function() my_matrix                      # Get cached matrix
                list(set = set, get = get,                              # List containing the various functions
                     setInverse = setInverse,
                     getInverse = getInverse)                         
        }

## This function computes the inverse of "my_matrix" 
## If the inverse has already been calculated (and "my_matrix" has not changed), then cacheSolve 
## should retrieve the inverse from cache.

        cacheSolve <- function(x, ...) {
                
                my_matrix <- x$getInverse()                                   # Retrive my_matrix cache
                if(!is.null(my_matrix)) {                                     # Check if cache exist, if yes - return my_matrix
                        message("Let's get us some cached data!")
                        return(my_matrix)                                     # Return inverse from cache
                }
                data <- x$get()                                               # If the cache does not exist, it gets "my_matrix"
                my_matrix_inv <- solve(data, ...)                             # Calculate the inverse of my_matrix
                x$setInverse(my_matrix_inv)                                   # Save result of inverse "my_matrix" in cache
                my_matrix_inv                                                 # Return the inverse of my_matrix as my_matrix_inv
                
        }



