# this function is creating a copy of a matrix passed as an argument
#the body of the function defines functions for the matrix to set it values, display its content, and find the inverse
# the function returns a list of all the functions defined for the the matrix
makeCacheMatrix <- function( x= matrix() ){   
	 m<- NULL
     	  set <- function(y) {
                x <<- y
                m <<- NULL
        }
	  get <- function() x
        inverse <- function(y) m <<-y
        getinverse <- function() m
        list(set = set, get = get,
             inverse = inverse,
             getinverse = getinverse)
}

# this function display the inverse of the matrix
#the body of the function checks if the inverse already exist in cache. If it does, it retrieves it, if not it calculates the inverse
# the function returns the inverse of the the matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$inverse(m)
        m
