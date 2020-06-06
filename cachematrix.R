## R Programming Week 3:Mike Woods-DeWitt
## Programming Assignment 2: Lexical Scoping 
## Matrix inversion is usually a computationally costly undertaking 
## there may be significant benefit to caching the inverse of the matrix rather than
## computing it repeatedly
## The functions below accomplish this task in two stages

## Stage 1 calculation
## Stage 1 creates creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                   initial <- NULL
                   setup <- function(y) {
                          x <<-y
                          initial <<-NULL
                   }
                   Return <- function () x
                   setup_inverse <- function(inverse) initial <<- inverse
                   Return_inverse <- function() initial
                   list(setup = setup, Return = Return, setup_inverse = setup_inverse,Return_inverse=Return_inverse)
  
}


## Stage 2 calculation
## Stage 2 computes the inverse of the special "matrix" returned in Stage 1 above
## Note If the inverse has already been calculated (and/or the matrix has not changed) then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
              initial <- x$Return_inverse()
              if (!is.null(initial)) {
                return(initial)
              }
              Matrix <- x$Return()
              initial <- solve(Matrix,...)
              x$setup_inverse(initial)
              initial
}
