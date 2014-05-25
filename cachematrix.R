

## Function returns 4 function that allow you to get/set the matrix/inverse
## the matrix and inverse are stored in state variables that persist outside the function scope.


makeCacheMatrix <- function(m = matrix()) {
    inv<-NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Check if inverse exists in cached variable inv, if so return it
## otherwise calc it using the solve function.. 

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- m$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data)
    m$setmean(inv)
    inv

}

