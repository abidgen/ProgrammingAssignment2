## "makeCacheMatrix" function takes only matrix as input. and "cacheSolve" 
## function calculates the inverse of the provided matrix (if possible) and 
## store it in the cache for later use.

## note: non-square matrix, square matrix with "zero" determinant value, 
## matrix containing character does not have any inverse matrix. 

## This function will take any matrix as input. If anything else other than 
## matrix is provided it not accept it. Also, if inverse of the matrix has been 
## already calculated using "cacheSolve" function, one can get the inverse 
## function without calculating it again as it has been already stored in the cache.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## this is the cache vector.
              ##upon execution of this function the cached value will reset. 
    
    if (!is.matrix(x)){
        x<-NULL 
        return(print('The input is not a matrix, please input a square matrix'))
        ## Here, it will checked if the input is a matrix or not. If not, it 
        ## will state it. cache value will be reset to NULL. 
    }
    set <- function(y) {
        if (is.matrix(y)){
            x <<- y
            m <<- NULL
        } else{
            print('The input is not a matrix, please input a square matrix')
        }
        ## Here one can rewrite the matrix  using 
        ##"makeCacheMatrix$set(new matrix)" and upon using "cacheSolve" again 
        ## the cacheed inverse matrix will be rewritten for the new matrix.
        ## anything other than matrix will be rejected and the previous cached 
        ## value will be available for calling.
    }
    get <- function() x # calls input matrix
    setinverse <- function(solve){
        m <<- solve
        ## stores the calculated inverse matrix by calculated by  "cacheSolve" 
        ## in cache vector "m" 
    } 
    getinverse <- function() m ## calls cached value of "m"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## this calls the cached value of m.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
        ## if the cached value is not NULL. It return cashed value and will not 
        ## do unnecessary calculation.
    }
    data <- x$get() 
    if  (length(data)==0){
        m<- print("No matrix is in cache")
        x$setinverse(m)
        ## if the input in "makeCacheMatrix" is not matrix and cached value has 
        ## been reset, this line will be executed. 
    } else if ((class(data[[1]])=='complex')&&(dim(data)[1]==dim(data)[2])){
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## if the input in "makeCacheMatrix" is a complex square matrix, the 
        ## inverse matrix will be calculated and stored in cache.  
    } else if ((class(data[[1]])=='numeric')&&(dim(data)[1]==dim(data)[2])&& 
               (det(data, ...)!=0)){
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## if the input in "makeCacheMatrix" is a numeric square matrix with a 
        ## non-zero determinant, the inverse matrix will be calculated and 
        ## stored in cache. 
    }else{
        m <- c(print("Inverse matrix is not possible for one of the following 
                     reasons:"),
               print("1. The matrix is not square matrix"),
               print("2. The determinat of the matrix is zero"),
               print("3. The matrix contains character value"))
        x$setinverse(m)
    }
    
}
        ## Return a matrix that is the inverse of 'x'

