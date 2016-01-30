## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function first sets the the matrix, then gets the matrix,
##then sets the inverse of the matrix and finally gets the inverse 
## of the matrix.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setmean <- function(inverse) inv <<- inverse
        getmean <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function either finds the inverse of the  matrix, or 
##retreives the previously cached inverse from memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
