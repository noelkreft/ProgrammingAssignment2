## INTRODUCTION
#  The goal of the two functions in this script is to make sure that the inverse
#  of a matrix is only calculated once and cached for future reference. 
 
## DESCRIPTION
#  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## DESCRIPTION
#  This function computes the inverse of the special "matrix" returned by 
#  `makeCacheMatrix` above. If the inverse has already been calculated 
#  (and the matrix has not changed), then `cacheSolve` should retrieve the 
#  inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


#################### TESTING FUNCTIONALITY ####################

#construct matrix object
mat<-makeCacheMatrix(matrix(c(1,0,0,1),2,2))

#check matrix data
mat$get()         

#get inverse: not present
mat$getinverse()

#calculate inverse
cacheSolve(mat)

#get inverse: inverse present
mat$getinverse()

#calculate inverse again: lookup from cache :)
cacheSolve(mat)

#set new matrix on same object
mat$set(mat<-makeCacheMatrix(matrix(c(1,0,0,-1),2,2)))

# check if inverse is empty again
mat$getinverse()

#calculate inverse: new inverse is calculated based on new matrix data
cacheSolve(mat)


