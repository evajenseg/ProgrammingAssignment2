## x is a square invertible matrix
## makeCacheMatrix is input to cacheSolve, where makeCacheMatrix can cache its inverse
## cacheSolve:computes the inverse of makeCacheMatrix (returned by makeCacheMatrix above). 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL			#object within function used later in function
	set<-function(y) {
		x<<-y			#assign to parent environment "<<-"
		inv<<-NULL		#assign to parent environment "<<-", This line of code clears any value of 
					#inv that had been cached by a prior execution cacheSolve
	}
	get<-function() x		#retrieves x from the parent environment 
		setinv = function(inverse) inv <<- inverse 	#defines the setter for the inv
       	getinv = function() inv					#assign input argument to the value of inv in the parent environment
		list(set=set, get=get, setinv=setinv, getinv=getinv)
			# gives the name 'set' to the set() function defined above, get to get
			# gives the name 'setinv' to the setinv() function defined above, 'getinv' to getinv() function

}

cacheSolve <- function(x, ...) {
	inv<-x$getinv()		#get the inv value from passed in argument
	if(!is.null(inv))	{	#if not NULL - no new value set, it gets the cache value
		message ("getting cached data")
		return (inv)
	}

	mat.data<-x$get()		# if (!is.null(inv)) is false, then it gets the matrix from input and calculate the inverse
	inv<-solve(mat.data,...)
	x$setinv(inv)
	return(inv)
}

