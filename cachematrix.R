##  guide on how to: 
## You can create a matrix using the following command: 
## m <- matrix(c(a, b, c, d), nrow= 2, ncol = 2)
## with a, b, c and d being the numbers of the vector. After that use   
## m2 <- makeCacheMatrix(m)
## and then get the inverse using the last function: 
## cacheSolve(m2)
## Try using the last function again, and you should receive the message "Getting 
## cached data", before receiving the inverse matrix. 

## The first function, the makeCacheMatrix function
## The function creates a matrix object that can cache its inverse. Using the 
## vector example as a guide on how to make the function and the design of the 
## function. 

makeCacheMatrix <- function(x = matrix()) {
      
      ## Cached inverse of a matrix
      inv <- NULL
      
      ##Get/set for matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      
      ## Now get/set for an inverse matrix 
      setinv <- function(inverse) inv <<- inverse 
      getinv <- function() inv
      
      ## return list of function for the matrix
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}

## The second function, the cacheSolve function
## A function that computes the inverse of a square matrix, and if the value
## have already been calculated and the matrix haven't changed, then it will 
## retrieve the inverse from cache. The vector example on the course is used as
## foundation, and the function will look very similar. 

cacheSolve <- function(x, ...) {
      
      ## Cached inverse of matrix
      inv <- x$getinv()
      
      
      ## A command to check for already computed, and thus return inverse matrix if true
      if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      
      ## if not true, then computes inverse of the matrix
      matr <- x$get()
      inv <- solve(matr, ...)
      
      ## Cache inverse 
      x$setinv(inv)
      
      ## Giving the value of the inverse matrix 
      inv
}
