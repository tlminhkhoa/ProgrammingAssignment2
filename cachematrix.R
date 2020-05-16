## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y){
                x <<- y
                inverseM <<- NULL
        }
        get <- function() { x }
        
        setInverse <- function(inverseArg){
                print("here")
                inverseM <<- inverseArg
        }
        
        getInverse <- function(){inverseM }
        
        return(list(set = set, get = get ,setInverse = setInverse, getInverse = getInverse))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseM <- makeCacheMatrix(x)$getInverse()
        if(!is.null(inverseM)){
                message("getting cached data")
                return(inverseM)
        }
        result <- solve(x)
        makeCacheMatrix(x)$setInverse(result)
        return(result)
}

test <- function(x) { y <<- x}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
