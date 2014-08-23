## The function will create a matrix that is capable of 
## caching its inverse.
makeCacheMatrix <- function(x = matrix()) {

	#initialize the inverse with null
        inv <- NULL

	#sets the value in the parent environment preserving the state beyond 
	#function environment.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	#gets the matrix itself
        get <- function() x

	#sets the inverse value to the inv parent environment 
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv

	#return list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cache solve function is responsible for checking if the cached information
## is available and returns it, if not available then generates inverse 
## of the matrix using solve function and sets it into 
## the matrix and returns the inverse.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	  inv <- x$getinv()

	#checking if its available in cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()

	 #generate inverse
        inv <- solve(data, ...)
        x$setinv(inv)

	#return inverse
        inv
}



