## The code caches possible time-eating computations 
## following the main instructions of the assignment

## Has a specific "matrix" that is able to cache inverse data
makeCacheMatrix <- function(k = matrix()){
      inv <- NULL
      set <- function(j){
            k <<- j
            inv <<- NULL
      }
      get <- function() {k}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## computes the inverse of the special "matrix" done by makeCacheMatrix
cachesolve <- function(k, ...){
      inv <- k$getInverse()
      if(!is.null(inv)){
            message("getting cache")
            return(inv)
      }
      mat <- k$get()
      inv <- solve(mat, ...)
      k$setInverse(inv)
      inv
}
