## The Creation of makeCacheMatrix and cacheSolve for ProgramAssignment2

## This function creates a special"matrix"object that can cache its inverse.
ewrwerwr
makeCacheMatrix <- function(x = matrix()) {
        
        # create the cached invert matrix
        inv<-NULL
        
        # define the set function
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        # define the get function
        get<-function()x
        
        # define the setinverse function
        setinverse<-function(inverse)inv<<-inverse
        
        # define the getinverse function
        getinverse<-function()inv
        
        #return the list of functions
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the special"matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # get the inverse of the matrix from the cache
        inv<-x$getinverse()
        
        # check if it is already cached
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # get the matrix data
        mat<-x$get()
        
        # solve the matrix data
        inv<-solve(mat,...)
        
        # set the inverse
        x$setinverse(inv)
        inv
}

