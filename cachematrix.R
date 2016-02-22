## Manipulates the scoping rules of the R langage to preserve the state inside
## of a matrix

## Creates a matrix in wich we can do the following operations :
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inv

makeCacheMatrix <- function(x = matrix())
{
        inv <- NULL
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## Returns a matrix that is the inverse of 'x'
## Checks to see if the inverse has already been calculated. If so, it gets the
##inverse from the cache and skips the computation. Otherwise, it calculates the
##inverse of the data and sets the value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...)
{
        inv <- x$getinv()
        if (!is.null(inv))
        {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
