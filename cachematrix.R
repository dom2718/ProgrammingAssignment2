## Two functions which create a matrix object which caches the inverse of the matrix 
## and compute the inverse of the matrix object only when necessary 
## i.e., when the matrix has chenged since the last computation of the inverse.



## The function makeCacheMatrux creates an object called a CacheMatrix which stores the matrix 
## and the value of the inverse and returns a list of 4 access functions set,get,setinv and getinv.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve returns the inverse of a CacheMatrix 
## The function cacheSolve returns the matrix inverse without computation if the cache is full
## otherwise the inverse is computed using the function solve , cached in the CacheMatrix and returned 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
    i<- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <-solve(data, ...)
    x$setinv(i)
    i
    
}
