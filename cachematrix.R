## This functions are created according to calculate the inverse of a matrix using the cache memory. As a consequence the operation will take less time than if we
## have to acces the local memory. This file has two functions, the first one is makecacheMatrix that is going to produce our special "Matrix", it is going to save it 
## in the cache memory, The second one is the function that allow us to calculate the inverse of the matrix using the cache memory of our PC. 

## In this function we are going to save the space of the cache memory to our matrix, we are going to set the space with the function set() and we are going to keep 
## space for the inverse with the function setInverse(). With the function get() and getInverse() we are going to obtain the value of the matrix and the value of the 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse= setInverse,
             getInverse = getInverse)
}


## In this function, we are going to calculate the inverse matrix with the function solve. First we are going to see if the inverse has been calculated.
## In the case of a negative answer, so the inverse has not been calculated, we are going to calculate it using the function solve. First we obtain the matrix from the 
## cache memory using the get() function, then we are going to find the inverse using the solve() function and then we are going to save the inverse in the cache with
## the setInverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
