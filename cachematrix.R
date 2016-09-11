## Aim is write two functions "makeCacheMatrix" & "cacheSolve" that will cache the in of a matrix

## `makeCacheMatrix`: This function creates a special "matrix" object
##  that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y){
x<<- y
inv <<- NULL
}

get<- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##  If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)){
		message("obtaining cached data")
		return(inv)
		}
		
		
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
