## The point this assignment is to program makeCacheMatrix and cacheSolve
## to get the inverse of a matrix

## makeCacheMatrix is able to make an object that caches the inverse of
## the inputted invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
+ inv <- NULL
+ set <- function(y) {
+   x <<- y
+   inv <<- NULL
+ }
+ get <- function() x
+ setinv <- function(inverse) inv <<- inverse
+ getinv <- function() inv
+ list(set = set, get = get, setinv = sentiv, getinv = getinv)
}

## cacheSolve is able to find the inverse of the matrix created by
## makeCacheMatrix. If the inverse was calculated previously, then the 
## cacheSolve retrieves said inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
+ inv <- x$getinv()
+ if(!is.null(inv)) {
+   message("getting cached result")
+   return(inv)
+ }
+ data <- x$get()
+ inv <- solve(data,...)
+ x$setinv(inv)
+ inv
}
## Checking the program
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)

## [,1]       [,2]       [,3]       [,4]
## [1,]  0.2764370 -0.3603314 -0.7287329  0.8631066
## [2,] -0.3939445  0.2964500 -0.5622493 -0.3105476
## [3,] -0.2545951 -0.1524979  0.4661964  0.4372265
## [4,] -0.2804694  0.5363282  0.5482504  0.2850941
