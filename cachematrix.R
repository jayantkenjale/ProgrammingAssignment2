## Following functions shows a implementaion to calcluate inverse of a matrix using R solve() function.
## It again ensures that once the Matrix is constructed using makeCacheMatrix. You can calcualte inverse 
## using cacheSolve() function where, cacheSolve() helps cache the expensive operation of solve()
## More details on their respective functions.

## You can call the functions as below 
## matrix <- makeCacheMatrix( matrix(c(4,2,2,4), nrow=2,ncol=2) )
## Then call cacheSolve() to calcualte inverse or return the cached value of inverse.
## cacheSolve(matrix)
## First time it will calculate the inverse but next time it will print a message "getting cached data"


## Function makeCacheMatrix() -
## This function makeCacheMatrix defines various functions within and act as a environment for 
## variable "x" which is original matrix and variable "inv" which holds its inverse.
## Its defines functions set(), get(), setInv() and getInv()
## 

makeCacheMatrix <- function(x = matrix()) {
    print(x)
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Function cacheSolve() -
## Takes matrix created using makeCacheMatrix() function
## cacheSolve() checks if the inverse is calculated earlier by calling getInv() function on passed matrix
## If the cache is aready present then it simply returns the value. Otherwise calculate the inverse of a matrix
## and set using setInv() function.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
