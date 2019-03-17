# The following are two functions which help to cache the inverse of a
# square matrix in a special object, so that it may later be retrieved and 
# used, rather than recalculate the inverse. The first function creates the 
# special object, while the second function checks to see if an inverse 
# is already cached. If there is a cached result, the second function returns
# that result, else it calculates the result, caches is and returns it.

## The first function: creates a special object, and takes a matrix as an arg.

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setinv <- function(inv) c <<- inv
        getinv <- function() c
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function: accepts an abject created using the above function 
## as an arg.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
