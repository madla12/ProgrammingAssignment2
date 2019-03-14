## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #makeCachMatrix with matrix argument creates 
    #   a list containing four functions:
    #set ... store the matrix in cache
    #get ... recall the matrix
    #setInverse ... store inverse of the original matrix
    #getInverse ... get inverse of the original matrix
    m <- NULL 
    #store matrix in cache
    set <- function(y){ 
        x <<- y 
        m <<- NULL 
    } 
    #get matrix
    get <- function() 
        x 
    #set inverse matrix
    setInverse <- function(solve)
        m <<- solve 
    #get inverse matrix
    getInverse <- function() 
        m 
    #list with four functions
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #Argument 'x' is an object created by makeCacheMatrix()
    m <- x$getInverse()   
    # 'm' is as check
    # if 'm' is NULL then function has to calculate inverse matrix using solve
    # if 'm' is not NULL then function returns inverse matrix from the cache  
    if(!is.null(m)){ 
        #info that inverse matrix has been already calculated and is in the cache
        message("getting cached data") 
        return(m) 
    } 
    #take only matrix from 'x'
    matrix <- x$get() 
    #computation of inverse matrix
    m <- solve(matrix, ...) 
    #set inverse matrix
    x$setInverse(m) 
    #'m' is now the same as x$getInverse()
    m
}
