## this function create the matrix object
## the init matrix is a empty matrix 

makeCacheMatrix <- function(x = matrix()) {
    ## m is the solve of the matrix which is initialisate at null
    m <- NULL
    ## initialisation of the matrix 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## visualisation of the matrix 
    get <- function() x
    ## assign the matrix inverse to m 
    setsolve <- function(solve) m <<- solve
    ## visualisation of the matrix inverse     
    getsolve <- function() m
    ## list of m 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## this function take in imput the objetc matrix compute the inverse
## if the inverse exist the function return the inverse 
## else function compute inverse and 
## assign the result to m 
cacheSolve <- function(x) {
    m <- x$getsolve()
    ## test if the matrix inverse exist
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    m <- solve(data) 
    x$setsolve(m)
    m
}