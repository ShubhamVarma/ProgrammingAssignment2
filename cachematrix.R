## Below are two functions that are used to create a special object
## that stores a numeric matrix and cache's its inverse.

## The function makeCacheMatrix creates a list containing a function to
## 1. Set the value of the vector
## 2. Get the value of the vector
## 3. Set the value of the mean
## 4. Get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    
    x_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(xinv) x_inverse <<- xinv
    
    getinverse <- function() x_inverse
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function cacheSolve calculates the inverse of the matrix created
## with the above function. If the inverse has already been calculated,
## it skips the coputation. Otherwise, it calculates the inverse using 
## the solve() function. The inverse is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    x_inverse <- x$getinverse()
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    data <- x$get()
    x_inverse <- solve(data, ...)
    x$setinverse(x_inverse)
    x_inverse
}
