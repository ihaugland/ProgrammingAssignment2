## These two functions creates a matrix, calculates the inverse of that matrix and caches the result. If the matrix alreday has an inverse cached, these functions also checks for that.

## makeCacheMatrix create a matrix and caches it

makeCacheMatrix <- function(x = matrix(...)) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve ##Set the solve function for all inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse, ##Sets the inverse of the matrix only
             getinverse = getinverse) ##Gets the inverse of the matrix
}


## cacheSolve returns the inverse matrix of matrix x

cacheSolve <- function(x, ...) {
        s <- x$getinverse() ##Searches to check if the inverse is set
        if(!is.null(s)) {	##Fetches the cached inverse
                message("getting cached data")
                return(s)
        }
        data <- x$get() ##Sets the data to be calculated to be that of the matrix
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
