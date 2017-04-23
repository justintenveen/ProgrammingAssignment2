## Put comments here that give an overall description of what your
## functions do
## These functions can cache the inverse of a matrix.

## Write a short comment describing this function
## The function makeCacheMatrix creates a special "matrix" that can cache its inverse.
## 1. set the value for the matrix
## 2. get the value for the matrix
## 3. set the value for the inverse of the matrix
## 4. get the value for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Write a short comment describing this function
## The function cacheSolve computes the inverse of the special "matrix" (above). If the inverse already has
## been calculated, it gets the inverse from the cache.
## In that case, the following message will be provided: "getting cached data".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
