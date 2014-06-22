## Put comments here that give an overall description of what your
## functions do

## This function will store the inverse of the matrix in m

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL                                     # sets m to NULL
    set <- function(m) {                          # creates function "set" that sets the matrix x equal to the matrix y
        x <<- y                                   # sets the matrix x global equal to the matrix y
        m <<- NULL                                # sets m globale equal to NULL. 
        
    }
    get <- function() x                           # creates a function "get" which contains the value of x. If get() is called the value of x is returned. 
    setinv <- function(inverse) m <<- inverse     # set m equal to the inverse value that is passed when setinv called.
    getinv <- function() m                        # What does this list do???
    list(set=set, get=get, 
         setinv = setinv,
         getinv = getinv)

}


## This function will calculate the inverse of the matrix passed to it or find the inverse of the matrxix if it exists in m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinv()                               # assigns m to the matrix that setinv function holds
    if(!is.null(m)) {                             # comparison. If m is null then it skips this function, if m has a value it returns the value
        message("getting cached data")
        return(m)
    }
    data <- x$get()                               # assigns data to the matrix value in get() function which should be the original matrix
    m <- solve(data)                              # assigns m to the inverse of the matrix
    x$setinv(m)                                   # assigns the value of setinv to the inverse matrix
    m                                             # returns m
}
