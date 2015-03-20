## Testing makeCacheMatrix
## a <- matrix(1:4, 2, 2) -> this will be our testing square matrix
## Run the makeCacheMatrix function 
## cm <- makeCacheMatrix(a) -> to make all functions available via cm$
## ia <- solve(a) -> to create an inverted matrix of a
## cm$getsolve() -> see that nothing is stored yet
## cm$setsolve(ia) -> store the inverted matrix
## cm$getsolve() -> check if the inverted matrix was well stored

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Here will my comments be
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
