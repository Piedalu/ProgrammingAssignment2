## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## cachematrix.r offer a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix())
{
    ## i will contains the inverse
    i <- NULL
    
    ## set function create a new makeCacheMatrix from a matrix
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    
    ## get function return the matrix
    get <- function() x
    
    ## setInverse function store the given inverse in i
    setInverse <- function(inverse) i <<- inverse
    
    ## getInverse function return the inverse stored
    getInverse <- function() i
    
    ## return a list containing all 4 functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...)
{
    ## Retrieving the value of the the inverse stored in x
    i <- x$getInverse()
    
    ## Evaluating if inverse as already been calculated
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    
    ## Calculating the inverse of x
    data <- x$get()
    i <- solve(data)
    
    ## Storing the inverse for further usage
    x$setInverse(i)
    
    return(i)
}