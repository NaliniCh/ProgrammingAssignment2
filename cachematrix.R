## Matrix inversion is a costly operation so it may help to cache the 
## inverse of a matrix than perform it repeatedly.
## Below two functions are used to create a special matrix that stores a 
## numeric matrix and cache its inverse


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored inverse value to NULL
    inv <- NULL

    # set value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL # Reassign changed matrix to NULL
    }
	
    # get value of matrix
    get <- function() x

    # set inverse of matrix
    setinverse <- function(inverse) inv <<- inverse

    # get inverse of matrix
    getinverse <- function() inv

    # return a list containing all functions defined above
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated then 
## the cacheSolve function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    # get inverse
    inv <- x$getinverse()
    
    # if inverse is already cached, return cached inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # if not, get matrix
    data <- x$get()

    # compute inverse of matrix
    inv <- solve(data, ...)

    # cache inverse of matrix
    x$setinverse(inv)

    # return inverse
    inv
}
