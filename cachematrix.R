## makeCacheMatrix stores a matrix in cache
## Testing makeCacheMatrix and it's operation
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

## cacheSolve displays the inverted/solved version of the matrix stored in makeCacheMatrix
## cacheSolve can be tested by running the function and then
## cacheSolve(cm)
## It will return the solved value of a
## try 
## cm2 <- makeCacheMatrix(a)
## followed by 
## cacheSolve(cm2) 
## to check for non-cached behaviour

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
