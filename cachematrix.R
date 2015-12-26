## makeCacheMatrix: create special kind of "matrix" to be used for cached solving

makeCacheMatrix <- function(x = matrix()) {
    # inverse_matrix: will hold inverse matrix value
    inverse_matrix <- NULL
    
    # set: use to update original matrix
    set <- function(y) {
        x <<- y
        # clear cached inverse matrix
        inverse_matrix <<- NULL
    }
    
    # get: get the original matrix values
    get <- function() x
    
    # setinverse: set calculated inverse matrix to be stored
    setinverse <- function(im) inverse_matrix <<- im
    
    # getinverse: return stored inverse matrix value
    getinverse <- function() inverse_matrix
    
    # return constructed object 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: return inverse matrix value, cache can be used

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # get original matrix
    data <- x$get()
    # calculate inverse matrix
    i <- solve(data)
    # store inverse matrix
    x$setinverse(i)
    # return inverse matrix value
    i
}
