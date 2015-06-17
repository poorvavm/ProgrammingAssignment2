makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set<-function(y){
    x<<-y
    inverse<<-NULL
}
get <- function() x
set<-function(inverse) 
    inv<<- inverse
getinverse<-function() inv
list(set = set, get = get, 
     setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv<-x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<- inverse(data, ...)
    x$setinverse(inv)
    return(inv)
}
        ## Return a matrix that is the inverse of 'x'
