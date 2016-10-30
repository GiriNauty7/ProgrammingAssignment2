# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x1= numeric()) {
            
            # stores cached value, if any otherwise holds NULL
            cache <- NULL
            
            # Store the matrix
            setMatrix <- function(temp_val) {
                        x1<<- temp_val
                        cache <<- NULL
                  }
            # get/ fetch the matrix
            getMatrix <- function() {
                        x1
                  }
           
            # cache the given argument
            cacheInverse <- function(solve) {
                            cache <<- solve
                    }
           
            # get the cached value
            getInverse <- function() {
                              cache
                      }
           
            # return a list with all above elements        
            list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
            }

  
cacheSolve <- function(x2, ...) {
  
            # get the value that has been cached
            inv_mat <- x2$getInverse()
            
            # if a cached value exists return it
            if(!is.null(inv_mat)) {
                      return(inv_mat)
            }
            
            # if matrix is not found, then get the matrix, compute the inverse
            # and save it in the cache of the system
            
            data <- x2$getMatrix()
            inv_mat <- solve(data)
            x2$cacheInverse(inv_mat)
                
            # return the inverse matrix
            inv_mat
        }
