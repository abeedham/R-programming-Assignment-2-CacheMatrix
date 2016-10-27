## There are two functions makeCacheMatrix and cacheSolve

## Function makeCacheMatrix
## This function creates a list containing a function to
##      get the value of the matrix 
##      set the value of the matrix 
##      get the inverted matrix 
##      set the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y #(assign from external environment)
                m <<- NULL #(assign from external environment)
        }
        get <- function() x     ## Get matrix to be inverted
        setinv <- function(inverse) m <<- inverse       ## Set inverted matrix 
        getinv <- function() m  ## Get inverted matrix 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function cacheSolve
## This function uses list generated in makeCacheMatrix (getinv, get, setinv)
## This function returns an inverted matrix
## The function checks to see if the inverted matrix already exists
## If it does, the cached inverted matrix is returned
## Otherwise the solve function in run to invert the matrix

cacheSolve <- function(x, ...) {
m <- x$getinv()                 ## m is assigned to getinv
if(!is.null(m)) {               ## if m is not null, this means m is cached
                message("getting cached data")
                return(m)       ## return m (getinv) i.e. cached inverted matrix
        }
        data <- x$get()         ## data is matrix to be inverted (get)
        m <- solve(data, ...)   ## solve function inverts matrix, assigns to m
        x$setinv(m)             ## cache inverted matrix m
        return(m)               ## return inverted matrix m
}




