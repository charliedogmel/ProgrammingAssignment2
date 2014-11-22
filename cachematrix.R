## Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
# not discuss here). Your assignment is to write a pair of functions that
# cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(matrix_object = matrix()) {
     matrix_inverse <- NULL
     setMatrix <- function(matrix_set) {
          matrix_object <<- matrix_set
          matrix_inverse <<- NULL
     }
     getMatrix <- function() matrix_object
     setInverse <- function(inverse) matrix_inverse <<- inverse
     getInverse <- function() matrix_inverse
     list(setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(cache_matrix, ...) {
     ## Return a matrix that is the inverse of 'cache_matrix'
     matrix_inverse <- cache_matrix$getInverse()
     if(!is.null(matrix_inverse)) {
          message("getting cached data")
          return(matrix_inverse)
     }
     matrix_object <- cache_matrix$getMatrix()
     matrix_inverse <- solve(matrix_object, ...)
     cache_matrix$setInverse(matrix_inverse)
     matrix_inverse
     
}
