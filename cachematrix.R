## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix() takes a matrix as an argument x and returns a list of getter and setter functions that get and set the argument x and inverse of the matrix.
## The function cacheSolve() takes an object of type makeCacheMatrix() as an argumnet, checks if the matrix had chnaged and if not, if the inverse is already present in the cache. If present, it directly returns the saved inverse, otherwise, the inverse is calculated and returned. 

## Write a short comment describing this function

## This function returns a list of functions that operate on the input matrix
makeCacheMatrix <- function(x = matrix()) { ## x is initialized to an empty matrix. This is done to ensure it won't produce any error if this function is called without an argument.
	inv <- NULL ## The inv is the varibale that stores the inverse of the x. It is initialized to NULL.
	set <- function(y){ ## Setter function to set the input matrix.
		x <<- y ## Here, x is in the environment of the parent function makeCacheMatrix() and so, to access that, the assignment operator "<<-" is used. So, x is set to the value passed to this function.
		inv <<- NULL ## inv is set to NULL every time the matrix is changed, which is done by the set() function.
	}
	get <- function() x ## Getter function that returns the value of x (which is in its parent environment).
	
	setinv <- function(i){ ## Setter function to set the inverse to the value i (passed to this function)
		inv <<- i ## Again, the operator "<<-" is used to assign the variable inv, which is in this functions' parent's environment.
	}
	getinv <- function() inv ## Getter function that returns the inverse of the matrix
	list(set = set, get = get, setinv = setinv, getinv = getinv) ## Returning a list that contains all the above four functions

}


## Write a short comment describing this function

## This function checks if the inverse is already present in the cache. If so, returns it, otherwise, calculates it and then returns it.
cacheSolve <- function(x, ...) { ## x is an object of type makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ## Get the current value of the inverse of the matrix
        if(!is.null(inv)){ ## If the inverse is not NULL, that means the inverse is already caclulated for this matrix and it is returned.
        	message("getting cached data")
        	return(inv)
        }
        
        ## This is when the inv is NULL
        mat <- x$get() ## Get the matrix from accessing the list method get().
        inv <- solve(mat) ## Calculate the matrix and store in inv.
        x$setinv(inv) ## Set the objects inv to this calculated inv.
        print("Returning inv")
        inv ## Return the inverse
        
}
