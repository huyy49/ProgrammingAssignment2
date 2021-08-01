# Below are two functions that are used to create a special "matrix" object 
# that stores a numeric vector and cache's its inverse.

# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function calculates the inverse of the special "vector" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## Return a matrix that is the inverse of 'x'
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Testing of functions created above
b1 <- matrix(1:4, 2, 2)
b1
b2 <- matrix(5:8, 2, 2)
b2
b1_i <- makeCacheMatrix(b1)
b1_i
b2_i <- makeCacheMatrix(b2)
b2_i
cacheSolve(b1_i)
cacheSolve(b2_i)
