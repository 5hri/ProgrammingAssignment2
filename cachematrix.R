## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## The file consists a pair of functions that cache the inverse of a matrix. The function uses <<- assignment operator 
## so that these internal variables are not exposed to the outside environment. 

## "The operators <<- and ->> are normally only used in functions, and cause a search to made through parent environments 
## for an existing definition of the variable being assigned. If such a variable is found (and its binding is not locked) 
## then its value is redefined, otherwise assignment takes place in the global environment."

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL          # Store the result of inversion
        set <- function(y) {
                x <<- y
                inv <<- NULL # Initialize xinv to null
        }
        get <- function() x # return the input matrix
        setInverse <- function(inverse) inv <<- inverse # set the inversed matrix
        getInverse <- function() inv # return the inversed matrix
        # return a list that contains these functions, makeCacheMatrix object contain
        # my_matrix <- makeCacheMatrix(test_matrix), do str(x) so that you can see the list of functions
        # my_matrix$set(new_matrix) # to change the matrix
        # my_matrix$get # to get the set matrix
        # my_matrix$setInverse # to set the inversed matrix
        # my_matrix$getInverse # to get the inversed matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()    # get the inversed matrix from object x
        # it will be null if it is not calculated
        # because in the function above "xinv <- NULL" in the previous function
        if (!is.null(inv)) { # check if the inversion result exists
                message("getting cached data") # message shown in console
                return(inv) # return the calculated inverted matrix
        }
        mat <- x$get() # if not, we do my_matrix$get() to get the matrix object
        inv <- solve(mat, ...) # Solve to get the inverse of matrix
        x$setInverse(inv) # Set the result to the object
        inv # return the solved result
}