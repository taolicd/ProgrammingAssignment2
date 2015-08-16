## The following functions are used cache the inverse of a matrix
## to speed up potentially time-consuming matrix inverse computations. 

## The first makeCacheMatrix function caches the inverse of a matrix
## first it sets the value of the matrix
## then it gets the value of the matrix
## thirdly it sets the inverse of the matrix
## lastly it gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The second cacheSolve function returns the inverse of a matrix
## then it checks to see if the inverse has already been calculated. 
##If so, it gets the reverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        
		inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

