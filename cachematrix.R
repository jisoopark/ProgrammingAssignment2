## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates and returns a list containing the set and get functions.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## assign the value of matrix to x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the matrix, x
    get <- function() x
    
    ## store inv to m
    setinverse <- function(inv) m <<- inv
    
    ## get the inversed matrix, m
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve takes the list returned by makeVector as a input, 
## reads the inverse of a matrix from cache if cache is not empty (NULL), and prints it out. 
## If the cache is empty (NULL), it will calculate the inverse matrix for the input,
## and cache it so that we can read it from cache next time.
cacheSolve <- function(x, ...) {
    ## check cache if there is the inverse of a matrix already
    inv <- x$getinverse()
    
    ## if cache is not empty, read the inverse from cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## get the matrix and assign it to data
    data <- x$get()
    
    ## calculate inverse for data and cache it
    inv <- solve(data)
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
