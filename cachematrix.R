# makeCacheMatrix can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # define the argument with default mode of "matrix"
  inv <- NULL # will hold value of matrix inverse 
  set <- function(y) { # define the set function to assign new 
    x <<- y
    inv <<- NULL
  }
  get <- function() x # define the get fucntion and return value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse # give value of inv in parent environment
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    
cacheSolve <- function(x, ...) { # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) { # if  inverse exists
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  # else  calculate the inverse of a matrix 
  inv <- solve(data)
  x$setinverse(inv) #  set the value of the inverse in the cache
  inv
}

##test
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()

cacheSolve(m)
cacheSolve(m)
