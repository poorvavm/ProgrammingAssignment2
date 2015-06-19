## This Program calculates the inverse of a matrix and saves it in a cache
## returns the cached value if one exists
## function to store and update cached matrix.
makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
#set matrix
set<-function(y){
    x<<-y
    inv<<-NULL
}
#get matrix
get <- function() x

#set inverse of a matrix
setinverse<-function(invmat) 
    inv<<- invmat
#get inverse of matrix
getinverse<-function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function calls getters and setters for cached value
## To check and update the cache data with inverse
cacheSolve <- function(x,...) {
# Return a inverse matrix of x
    inv<-x$getinverse()
#return value if not null
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
#calculate the iverse and savve it in cache
    data<-x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
}
## Return a matrix that is the inverse of 'x'

## Sample test run results
## -------------------------------------------------
## > testmatrix <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2, byrow = T)
## >
## > testmatrix
##        [,1] [,2]
##  [1,]    1    2
##  [2,]    3    4
## >
## > cachedmatrix <- makeCacheMatrix(testmatrix)
## > cacheSolve(cachedmatrix)
##      [,1] [,2]
##  [1,] -2.0  1.0
##  [2,]  1.5 -0.5
## > cacheSolve(cachedmatrix)
## getting cached data
##      [,1] [,2]
##  [1,] -2.0  1.0
##  [2,]  1.5 -0.5
