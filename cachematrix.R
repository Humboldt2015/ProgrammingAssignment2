## Two functions are being created that cache the inverse of
## a matrix and such that next time the user calculates the matrix
## inverse, the previously calculated and saved value is returned
## instead of calculating the value again.

## The first function, makeCacheMatrix, creates a special "matrix" object "x"
## and some sub-functions, which is a list containing the function that
## performs the following:
## 1.- sets the value of the matrix
## 2.- gets the value of the matrix
## 3.- sets the value of the inverse
## 4.- gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## defines the cache m
        set <- function(y) {
                x <<- y ## assigns the imput matrix y to the variable x in
                        ## the parent environment
                m <<- NULL ## re-initializes m in the parent environment to null
        }
        get <- function() x ## returns the matrix x
        setinverse <- function(inverse) m <<- inverse ## sets the cache m equal
                                                      ## to the inverse of the
                                                      ## matrix x
        getinverse <- function() m ## returns the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The next function, cacheSolve, calculates the inverse of the special matrix
## created in the previous function.  It first checks if the inverse has been
## already calculated.  If this is the case, it gets the inverse from the
## cache and skips the calculation.  If not, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) { ## returns a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}