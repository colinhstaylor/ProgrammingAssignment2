## Functions to cache the inverse of matrices to avoid repeat computations
## Matrix and its inverse stored in a list

## makeCacheMatrix converts a matrix object into a list suitable for passing to
## cacheSolve function
## Can be called on as many matrix objects as needed without interference

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # create object to store inverse and set to NULL
        set <- function(y) { # edit the matrix and reset the inverse
                x <<- y
                inv <<- NULL
        }
        get <- function() x # returns the matrix
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse) # list of 4 functions
        # 'x' and 'inv' values are stored in the "execution environment", where
        # they are defined -- for more info on how and why this works
        # see http://adv-r.had.co.nz/Environments.html
}


## cacheSolve takes return value of makeCacheMatrix as argument
## Returns the inverse of the matrix
## Computation is only needed once for any given list

cacheSolve <- function(x, ...) { # ... are any additional arguments for 'solve'
        inv <- x$getinverse()
        # Checks whether inverse has been computed before
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv) # exits here if 'if' statement is TRUE
        }
        # if not, the rest of the function runs:
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}