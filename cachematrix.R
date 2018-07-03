## These functions are used to calculate the 
## inverse of a matrix and cache it.
## The cached inverse can be recovered
## without incurring in repeat computation.



## creates a list containing a function to
## 1) set the content of matrix
## 2) get the contentof the matrix
## 3) set the content of the inverse matrix
## 3) get the content of the inverse matrix

makeCacheMatrix <- function(X = numeric()) {
        Xinv <- NULL
        set <- function(Y) {
                X <<- Y
                Xinv <<- NULL
                
        }
        get <-function() X ##may need to parse as vector
        setinv <- function(solve) Xinv <<- solve ##may need to parse as vector
        getinv <- function() Xinv
        list (set = set, get = get,
              setinv = setinv, getinv = getinv)

}


## calculates the inverse of the special 
## matrix created with the above function
## or recovers the existing cached inverse.

cacheSolve <- function(X,...) {
        
        ## Return a matrix that is the inverse of 'X'
        
        Xinv <- X$getinv()
        if (!is.null(Xinv)) {
                message("getting cached inverse")
                return(Xinv)
        }
        data <- X$get()
        Xinv <- solve(data,...)
        X$setinv(Xinv)
        Xinv
        
}
