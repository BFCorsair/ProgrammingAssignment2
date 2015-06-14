## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix stores a copy of its input matrix and its inverse
## in the global environment
## Returns a list of 4 functions to write / read the input matrix/its inverse
makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL  # iv is the cached value of the inverse- when it exists
        # not used here - allows to change the value of the matrix
        set <- function(y) {  
                x <<- y 
                # when the value is changed, the inverse has to be reset
                iv <<- NULL  
        }
        get <- function() x  # returns a matrix equal to the input matrix
        # setinverse stores the argument in the cache of the inverse
        setinverse <- function(invrs) iv <<- invrs
        getinverse <- function() iv  # retrieves the cache of the inverse
        # return the list of 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	

}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix stored
## in a CacheMatrix object
## If the inverse has already been computed, cacheSolve returns the cached value
## If not, it computes the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix iv that is the inverse of 'x'
        iv <- x$getinverse()  # attempt to get the cached version
        if(!is.null(iv)) {  # iv is not NULL - so we got the cached version
                message("getting cached data") # we're done
                return(iv)
        }
        ## Cache miss -> have to compute
        matrix <- x$get()  # get the matrix itself
        iv <- solve(matrix, ...)  # compute the inverse
        x$setinverse(iv)  # cache the resuls
        iv  # return the inverse of x
}
