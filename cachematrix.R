## get() gets the value of the matrix under consideration x
## set() sets the value of the matrix, by default it's x. In case we modify the values of the matrix, 
## the inverse xInverse will also change and therefore will be set to NULL. 
## setinverse() sets the value of the matrix inverse
## getinverse() gets the values of the matrix inverse
## makeCacheMatrix() returns a special matrix object (list of the 4 functions described above)
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        
        xInverse <- NULL
        
        get <- function(){                                 
                
                x
        }
        
        set <- function(mY = x, ...){
                x <<- mY
                xInverse <<- NULL    
                
        }
        
        setinverse <- function(xInverse) xInverse <<- xInverse
        getinverse <- function()          xInverse
        
        list(get = get, set = set, 
             setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve calculates the inverse of "special" matrix returned by makeCacheMatrix
## if it already has been calculated (and the matrix has not changed), it will get the matrix inverse immediately from the cache
## if not, it will calculate the matrix inverse and 
## it will set the value of the matrix inverse in the cache using the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        xInverse <- x$getinverse()
        
        if(!is.null(xInverse)){
                message('getting the cached inverse matrix')
                return(xInverse)
        }
        
        x$set()
        data            <- x$get()
        xInverse        <- solve(data)
        x$setinverse(xInverse) ## Put it into the cache      
        xInverse
        
}
