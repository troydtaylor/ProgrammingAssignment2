makeCacheMatrix <- function(x = matrix()) {
	# Per the assignment: This function creates a special "matrix" object that
	# can cache its inverse. 
	m<-NULL #initialize the matrix
    # The <<- allows assign value to an environment outside than this 
	# function IE lexically scoped
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    #now create a variaion of solve function to set the inv matrix
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x = matrix(), ...) {
    ## Per the assignment: This function computes the inverse of the special 
	## "matrix" returned by makeCacheMatrix above. If the inverse has already
	## been calculated (and the matrix has not changed), then the cachesolve 
	## should retrieve the inverse from the cache - IE - Return a matrix that
	## is the inverse of 'x'
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
