## The makeCacheMatrix function will create a "special" matrix that can store the inverse of the input
## matrix (x)
##the CacheSolve function will check if the inverse is already calculated, if not then it will calculate
## it and store it in (x)
## no error checking in input matrix exists, test with 
##
## x = rbind(c(13, -13), c(-10, 12))
## m<- makeCacheMatrix(x)
## cacheSolve(m)
##
## or similar

makeCacheMatrix <- function(x = matrix()) { 
        inverse <- NULL ## inverse is set to null (nothing)
        set <- function(y) {
                x <<- y
                inverse<<- NULL
        }
        get <- function() x ##function that returns the matrix X
        setinversematrix <- function(inversematrix) inverse <<- inversematrix ##function that sets the inversematrix
        getinversematrix <- function() inverse ##function that gets the inversematrix
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinversematrix()         ## retrieve the inversematrix
        if(!is.null(inverse)) {                 ## if the inversematrix cache exists, return it...
                message("getting cached data")
                return(inverse)
        }
        else{                                  ## if the inversematrix cache does not exists, calculate it...
                message("Calculating inverse matrix and storing it in cache")
                data <- x$get()
                inverse <- solve(data, ...)
                x$setinversematrix(inverse)
                return(inverse)}
}
