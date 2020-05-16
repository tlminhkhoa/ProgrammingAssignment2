## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
#         function to set x
        set <- function(y){
                x <<- y
                inverseM <<- NULL
        }
#       function to get x
        get <- function() { x }
#       function to set the inverse 
        setInverse <- function(inverseArg){
                inverseM <<- inverseArg
        }
#       function to get the inverse
        getInverse <- function(){inverseM }
        
        return(list(set = set, get = get ,setInverse = setInverse, getInverse = getInverse))
}


## Write a short comment describing this function
# The function first check if the inverse exist or not if yes it return it
# If the inverse is not there, it will calculate and store the inverse
# then return it to the user
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
