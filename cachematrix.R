## the functions makeCacheMatrix and cacheSolve compute the inverse of the
## matrix, x (the parameter of makeCacheMatrix), 
## and stores it and its inverse in a (calling) environment so that the 
## inverse can be recalled from the environment, instead of re-calculating it
##
## EXAMPLE USE: suppose nice.matrix is a square invertable matrix
##
## > current.problem <- makeCacheMatrix(nice.matrix)
##
## nice.matrix is stored in the calling environment that creates current.problem
## 
## > cacheSolve(current.problem)
## 
## returns the inverse of current.problem

## the function makeCacheMatrix does the following
## - takes as input a square invertible matrix
## - creates the calling environment within which are stored
## -- the inputed matrix (must be square and invertable)
## -- its inverse and
## -- four functions that set and get the matrix and its inverse
##    from the function's calling environment
## - returns a list of the functions 

makeCacheMatrix <- function(x = matrix()) {
    ## NOTE: the default value of x is an empty matrix
    ## NOTE: the user MUST input a square matrix, else an ERROR will occur
    ## NOTE: i.e. Error in solve.default 'a' must be square
    ## x and inv.matrix are stored within the function's calling environment

    ## initialize inv.matrix,
    inv.matrix <- NULL

    ## set: initialize x and inv.matrix
    ## NOTE: set is not called within the makeCacheMatrix and cacheSolve
    ##  it is used to re-initialize the cached values, i.e.
    ##    current.problem$set(new.nice.matrix)
    ##    cacheSolve(current.problem)
    ##  current.problem now contains the inverse for new.nice.matrix
    set <- function(y) {
        x <<- y
        inv.matrix <<- NULL
    }

    ## get: return x
    get <- function() x

    ## setinverse: assign inv.matrix
    ## <<- assigns inv.matrix within setinverse's parent environment
    setinverse <- function(inverse) inv.matrix <<- inverse

    ## getinverse: return the object inv.matrix
    getinverse <- function() inv.matrix

    ## make a list composed of the 4 functions just defined
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## INPUT: 4-element list created by makeCacheMatrix 
## PROGRAM: 
##  - if not stored calculate the inverse of the matrix 
##  - else retrieve the inverse from the environment
##  - return the inverse

cacheSolve <- function(x, ...) {

    ## the parameter x is a 4-element list defined by makeCacheMatrix
    ##  it contains the functions getinverse, get, setinverse, set
    ## ...: is the set of parameters that can be passed to solve

    ## retrieve the object inv.matrix from the environment of the parameter x
    inv.matrix <- x$getinverse()

    ## if inv.matrix is populated then retrieve and return
    if(!is.null(inv.matrix)) {
        message("getting cached data")
        return(inv.matrix)
    }

    ## the rest of the routine is executed only when the previous condition
    ## is false, i.e. inv.matrix is not populated
    
    ## get the data from the environment of x
    data <- x$get()

    ## solve for the inverse
    inv.matrix <- solve(data, ...)

    ## cache the inverted matrix
    x$setinverse(inv.matrix)

    ## return the inverted matrix
    inv.matrix
}
