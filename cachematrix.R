
## update matrix and get it in chache.
makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set<-function(y){
    x<<-y
    inv<<-NULL
}
get <- function() x
setinverse<-function(invmat) 
    inv<<- invmat
getinverse<-function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## To check and update the cache data with inverse matrix...
cacheSolve <- function(x,...) {
    inv<-x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
}
## Return a matrix that is the inverse of 'x'

