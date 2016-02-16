## Make vache matrix object

makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x)) {
        warning("x must be matrix!")
        return(NaN)
    }
    
    inversematrix <- NULL
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    get <- function() x
    
    set_inversematrix <- function(new_inversematrix) { 
        inversematrix <<- new_inversematrix
    }
    get_inversematrix <- function() inversematrix
    
    list(set = set, get = get,
         set_inversematrix = set_inversematrix,
         get_inversematrix = get_inversematrix)
}


## Get inverse matrix from cache or calculate it

cacheSolve <- function(x, ...) {
    if (is.null(x) || is.na(x)) {
        warning("x is undefined!")
        return(NaN)
    }
    
    data <- x$get()
    if (nrow(data) != ncol(data)) {
        warning("Matrix is not invertable: (num rows != num cols) !")
        return(NaN)
    }
    
    if (det(data) == 0) {
        warning("Matrix is not invertable: (determinant == 0)!")
        return(NaN)        
    }
    
    inversematrix <- x$get_inversematrix()
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    
    inversematrix <- solve(data, ...)
    x$set_inversematrix(inversematrix)
    
    inversematrix
}