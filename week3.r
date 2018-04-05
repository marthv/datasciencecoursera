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

cacheInverse <- function (x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

makeCachematrix <- function (x = matrix()){
  inv <- NULL
  set <-function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list (set = get, get = get, setinverse = setinverse, getinverse = getinverse)
}


c <- matrix(c(1,4,3,2), nrows = 2, ncols = 2)
d <- c(1,4,3,2)
a <- makeVector(d)
a
b <- cachemean(a)
b

e<- makeCachematrix(c)
e
f <- cacheInverse(e)
f

