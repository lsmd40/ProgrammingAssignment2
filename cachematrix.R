## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix()
## This function creates a special "matrix" object that can cache its inverse
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve
## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
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

## Example output:
## > z <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), 3, 3)
## > z
##      [,1] [,2] [,3]
## [1,]    1    0    5
## [2,]    2    1    6
## [3,]    3    4    0
## > matrix <- makeCacheMatrix(z)
## > matrix$getinverse()
## NULL
## > cacheSolve(matrix)
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## > cacheSolve(matrix)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## > matrix$getinverse()
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
