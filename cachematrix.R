#ProgrammingAssignment2
## Write a short comment describing this function
## This function takes a matrix and creates a series of setters and getters
## for creating an inverse matrix.  This function doesn't actually create the inverse.
## The inverse gets created using the solve() function in the cacheSolve function.
## A list is returned providing access to the setter and getter.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) m <<- inverse
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function uses the setter and getters from the makeCacheMatrix
## to create an inverse of the Matrix.  It checks if the inverse of the original 
## matrix has already been created.  If it has, it will return the inverse matrix.
## If it hasn't, it'll create the inverse, set it to the setInverse() function from 
## the makeCahceMatrix(), and then return the inverse of the original function.
    
cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached Invertible Matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}

##testing
Test1 <- matrix(1:4,2,2)
Test1

CacheM <- makeCacheMatrix(Test1)
CacheM$get()

Test2 <- cacheSolve(CacheM)
Test2
solve(Test1) 

Test1 %*% Test2

Test1 <- rbind(c(1,-1/4),c(-1/4,1))
Test1
CacheM <- makeCacheMatrix(Test1)
CacheM$get()


Test2 <- cacheSolve(CacheM)
Test2
solve(Test1)

Test1 %*% Test2
