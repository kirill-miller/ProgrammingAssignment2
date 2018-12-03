## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. makeCasheMatrix creates a special "matrix", 
## which is really a list containing a function to
##	1	set the value of the matrix
##	2	get the value of the matrix
##	3	set the value of inverse of the matrix
##	4	get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## casheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from cache. 

cacheSolve <- function(x, ...) {
		mat <- x$get()
		if(nrow(mat)!=ncol(mat)){
			return(message("matrix must be square."))
		} else if(det(mat)==0){
			return(message("determinant equal to zero. matrix is not invertable."))
		}
		
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}

## Sample run:
##
## > a <- matrix(1:4,2,2)
## > b <- matrix(c(5,8,9,0),2,2)
## > c <- matrix(rep(1,6),2,3)  
## > d <- matrix(1:10000,100,100)
## > m1 <- makeCacheMatrix(a)
## > m2 <- makeCacheMatrix(b)
## > m3 <- makeCacheMatrix(c)
## > m4 <- makeCacheMatrix(d)
## > m1$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m1$getInv()
## NULL
##
## No cache in the first run
##
## > cacheSolve(m1)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > m1$getInv()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## From the cache in the second run
##
## > cacheSolve(m1)
## getting cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Check of matrix inversion
##
## > a %*% cacheSolve(m1)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## > m2$get()
##      [,1] [,2]
## [1,]    5    9
## [2,]    8    0
## > m2$getInv()
## NULL
##
## No cache in the first run
##
## > cacheSolve(m2)
##           [,1]        [,2]
## [1,] 0.0000000  0.12500000
## [2,] 0.1111111 -0.06944444
## 
## From the cache in the second run
##
## > cacheSolve(m2)
## getting cached data
##           [,1]        [,2]
## [1,] 0.0000000  0.12500000
## [2,] 0.1111111 -0.06944444
## 
## > m2$set(matrix(c(2, 2, 1, 2), 2, 2))
## > m2$get()
##      [,1] [,2]
## [1,]    2    1
## [2,]    2    2
## > cacheSolve(m2)
##      [,1] [,2]
## [1,]    1 -0.5
## [2,]   -1  1.0
## > cacheSolve(m2)
## getting cached data
##      [,1] [,2]
## [1,]    1 -0.5
## [2,]   -1  1.0
##
## > cacheSolve(m3)
## matrix must be square.
## > cacheSolve(m4)
## determinant equal to zero. matrix is not invertable.