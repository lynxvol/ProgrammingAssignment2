## Function create a special vector that store the matrix and its
## cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set the matrix value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #retrieve the stored matrix value
    get <- function() x
    
    #compute and save the inverse matrix value
    setinverse <- function(solve) m <<- solve
    
    #retrieve the stored inverse matrix value
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function below will take the matrix create from makeCacheMatrix and
## return its inverse matrix
cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    #check if the inverse matrix already cached and return its value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # line below will execute to create the cached inverse matrix if 
    # there is no cached data available
    data <- x$get()         #get the matrix value
    m <- solve(data)        #compute the inverse matrix
    x$setinverse(m)         #store/cache the inverse matrix
    m                       #print out the inverse matrix
}