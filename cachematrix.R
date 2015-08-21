## We create a function that cache in a different environment a matrix and cache its inverse 
##  so that it can be recalled without being calculated any time it is needed

## This function create a matrix and can cach its inverse in a different environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setminv <- function(solve) m <<- solve
        getminv <- function() m
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)

}


## this function can calculate the inverse of the matrix previously defined, but first check if this is already existing

cacheSolve <- function(x, ...) {
        m <- x$getminv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setminv(m)
        m


        ## Return a matrix that is the inverse of 'x'
}
