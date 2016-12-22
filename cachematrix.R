## This funciton takes the inverse of a matrix


##This function takes the inverse of a matrix and stores it as a variable

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list( set = set, get = get,
              setinv = setinv, getinv = getinv)
}


##This function checks to see whether the inverse for the matrix has already
##been calculated and stored and if so, returns that the stored variable. 
##Otherwise it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
       if (!is.null(m)) {
               message("getting cached data")
               return(m)
        } 
        data <- x$get() ## Return a matrix that is the inverse of 'x'
        m <- solve(data, ...)
        x$setinv(m)
        return(m)
}