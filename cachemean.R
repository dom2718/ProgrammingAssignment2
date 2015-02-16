# makeVector creates an object which includes a vector data and 
# functions which access and make computations on the object
# note that the data value is reset using the <<- assignment 
# all access to the data must use get and set and 
# whenever the mean is computed with getmean the value is saved (CACHED)
# whenever the dats is changed using set the mean is reset to NULL

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# cachemean assumes that the object x was created by makeVector 
# when called the function returns the mean immediately if x has been unchanged 
# otherwise the mean is recomputed and the value cached,
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
