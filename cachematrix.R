#ProgrammingAssignment2
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