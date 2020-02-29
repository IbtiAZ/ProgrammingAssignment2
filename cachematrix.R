## The functions bellow are constructed in a similar fashion to the ones on the provided example. 
## The first function creates a special "matrix" object that can cache the inverse of a matrix 
makeCacheMatrix <- function(mat = matrix()) {
    invmat <- NULL
    set <- function(d) { 
      mat <<- d
      invmat <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## The following function, computes the inverse of the special "matrix returned by the makeCacheMatrix. 
## If the inverse was computed before the cacheSolve retrieves the inverse form the cache. 
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
    invmat <- mat$getinverse()
    if(!is.null(invmat)) {
      message("getting cached data")
      return(invmat)
    }
    matrix <- mat$get()
    ##Solve() is used for computing the inverse of a square invertible matrix
    invmat <- solve(matrix, ...)
    mat$setinverse(invmat)
    invmat
}
## Code Test 
trial_matrix <- makeCacheMatrix(matrix(rnorm(9),3,3))
trial_matrix$get()
cacheSolve(trial_matrix)