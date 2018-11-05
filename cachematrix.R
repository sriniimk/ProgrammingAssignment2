
setwd("C:/Users/svadde/Desktop/PSS/Data Science/Coursera/Data/2. R Programming/")

#The first function, makeVector creates a special "vector", which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean
	
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

#The following function calculates the mean of the special "vector" created with the above function. 
#However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache 
#and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache 
#via the setmean function.

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


#====Qustion 1
#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mtrx = matrix()) {

  #Initialize the inverse property
  invrse <- NULL

  #Set matrix
  set <- function(matrix) 
  {
	mtrx <<- matrix
	invrse <<- NULL
  }

  #Get matrix
  get <- function() 
  {
    #Return the matrix
	mtrx
  }

  #Set inverse of the matrix
  setInverse <- function(inverse) 
  {
	invrse <<- inverse
  }

  #Get inverse of the matrix
  getInverse <- function() 
  {
	#Return the inverse property
	invrse
  }

  #Return a list of the methods
  list(set = set, get = get,
	   setInverse = setInverse,
	   getInverse = getInverse)
}


#====Qustion 2
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(mtrx, ...) {
        ## Return inverse of the data stored in mtrx variable
    invrse <- mtrx$getinverse()

    if(!is.null(invrse)) {
        message("getting cached data")
        return(invrse)
    }
    data <- mtrx$get()
    invrse <- solve(data, ...)
    mtrx$setinverse(invrse)
    invrse
}
