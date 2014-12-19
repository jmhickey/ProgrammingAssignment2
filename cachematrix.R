## These functions create a special wrapper
#  for a matrix, which caches the result of the solve
#  function used to compute the inverse of the given matrix.

## makeCacheMatrix -- returns a list which serves as a wrapper of the given
#                     matrix.  The list contains getters and setters for the matrix
#                     as well as a getter and setter for the matrix's inverse.
#       params: x --  the matrix to wrap
makeCacheMatrix <- function(x = matrix()) {
    #internal variable to store the matrix's inverse
    i <- NULL
    
    #Store the matrix and clear the inverse variable
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #Retrieve the matrix
    get <- function() x
    
    #Store the inverse for the matrix
    setinverse <- function(inverse) i <<- inverse
    
    #Retrieve the inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve -- returns the inverse of the given CachedMatrix.  Retrieves the inverse from
#                the cache if it has been previously computed.  If the cached value is null,
#                it computes the inverse using solve, updates the cache and then returns the result.
#       params: x -- the CachedMatrix, which must be the result of wrapping an invertible matrix
#                    with a call to the makeCacheMatrix function
#               ... -- additional arguments which are passed to the solve method.  Ignored if the inverse has been
#                      previously computed and cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #Get the result from the CachedMatrix
    i <- x$getinverse()
    
    #Return the result if previously computed
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    #Otherwise, get the underlying matrix
    #compute the inverse, cache it and return it
    stored_matrix <- x$get()
    i <- solve(stored_matrix, ...)
    x$setinverse(i)
    i
}