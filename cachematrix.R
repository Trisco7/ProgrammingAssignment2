### Two functions have been written to solve the assignment problem. 
### The problem is to cache the inverse of a given matrix and display the 
### cache result instead of calculating again and again whenever the 
### inverse for a given matrix needs to be found out. For this purpose, 
### two functions have been written.
### 1.makeCacheMatrix: This function creates a special "matrix" object that 
### can cache its inverse.
### 2.cacheSolve: This function computes the inverse of the special "matrix" 
### returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), 
### then the cachesolve should retrieve the inverse from the cache.


## The function "makeCacheMatrix()" takes a matrix as its input and assumes that
## the matrix supplied is always invertible. The function creates a special 
## "matrix" object and returns a list of members and/or methods such as 
## set(): to set the value of matrix
## get(): to get the value of matrix
## setinverse(): to set the inverse value of matrix
## getinverse(): to get the inverse value of matrix
## The "<<-" operator has been used in set() and setinverse() functions causing a 
## search to made through parent environments for an existing definition of the 
## variable being assigned. If such a variable is found, its value is redefined 
## instead of allocating new space to the variables.
## The variables returned in get() and getinverse() functions get respective 
## values due to lexical scoping rules followed in R.

makeCacheMatrix <- function(x = matrix()) {
       inverseMatrix <- NULL
       set <- function(y) {
                x <<- y
                inverseMatrix  <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) inverseMatrix  <<- inverse
       getinverse <- function() inverseMatrix
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

} #makeCacheMatrix 


## The function "cacheSolve()" takes the special "matrix" object created in the 
## function "makeCacheMatrix()" as its input and returns the inverse of the matrix 
## stored in the object. First time, the function computes the inverse of the matrix 
## retrieved using the member function get() of the list object and set the inverse 
## in the object by accessing the member function setinverse() of the list. 
## Then for each next time, if matrix has not been changed, the cached inverse result 
## is retrieved using the member function getinverse() of the list object. 
## (This function returns NULL the very first time. Hence, inverse needs to be 
## calculated using solve())

cacheSolve <- function(x, ...) {
        
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
} #cacheSolve 
