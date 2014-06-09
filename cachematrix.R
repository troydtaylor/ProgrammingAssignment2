## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL #initialize the matrix
    #The <<- allows assign value to an environment other than this function
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    #now create a variaion of our solve function to set the inv matrix
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getsolve()
    # Here we check if the matrix m exists and return it if it does
    # The return will exit you from the function
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    #otherwise we will go ahead and calc it and store it
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setsolve(m)
    #return the matrix m
    m
}
