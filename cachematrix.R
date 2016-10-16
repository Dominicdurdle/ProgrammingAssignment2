## These functions cache the inverse of a matrix. This is done to avoid unneccesarily 
## recalculating the inverse and saving programming time. 

##This function creates a special "matrix", which is required to cache its inverse.

makeCacheMatrix<-function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) inv <<- solve
        getinverse<-function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}        


## The following function returns a matrix, that is the inverse of 'x'. It only calculates the inverse
## if an inverse has not already been calculated. Otherwise it returns the cached inverse matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        return(inv)
}
