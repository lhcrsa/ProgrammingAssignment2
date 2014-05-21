##The cacheSolve function can be used to return the inverse of a matrix.
##Efficiency is provided by caching any inverse using the makeCacheMatrix:
##  An efficient approach is used in that once the matrix inverse is calculated
##   it is cached by the makeCacheMatrix method.  Future access to the
##   matrix inverse then comes from the cached version avoiding duplication of
##   possibly expensive matrix inverse calculations.

##makeCacheMatrix: Input parameter, x, an invertible matrix.
##  Returns: List of functions providing access cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	m.inv = NULL;  #inverse of matrix
	#return a list of functions: set, get, setinverse, getinverse
	list (
		set = function(y) {
			x <<- y
			m.inv <<- NULL
		},
		
		get = function() { x },
		
		setinverse = function(minv) { m.inv <<- minv},
		
		getinverse = function() { m.inv }
	)
}


##cacheSolve: Input parameter, x, an invertible matrix
##  Returns: the inverse of the matrix x.  
cacheSolve <- function(x, ...) {
        #Access then test to see if the cache holds a previous inversion
        m = x$getinverse()
        if (!is.null(m)) {
        	#message("getting cached inverse")
        	#return cached version
        	return(m)
        }
        
        #no cached inversion, so calculate matrix inverse and store in cache
        data = x$get() #get the original matrix
        m = solve(data)
        x$setinverse(m) #store inverse in the cache
        m
}
