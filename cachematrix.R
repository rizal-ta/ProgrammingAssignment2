## makeCacheMatrix() and cacheSolve() are used to compute inverse of the matrix
## but if there is a cached inverse matrix we can avoid computing and get the
## answer from the cache.


## makeCacheMatrix() takes a matrix object as its argument and returns a list of
## four functions. 
## 1. set() to set the matrix 
## 2. get() to get the matrix 
## 3. setinverse() to set inverse of the matrix 
## 4. getinverse() to get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve() returns the inverse of the matrix set by the makeCacheMatrix().
## Before it computes  the inverse it checks if there is a cached inverse matrix
## and returns the cached version if there is one.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setinverse(i)
    i
}
