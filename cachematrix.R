##The functions bellow cache the inverse of a matrix and retrieve the result

##makeCacheMatrix creates a matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()){  ##
  mi <- NULL
  set <- function(y){
    x<<- y
    mi<<-NULL
  }
  get <- function() x
  seti <- function(solve) mi<<- solve
  geti <- function() mi
  list(set=set, get=get, seti=seti, geti=geti)
}

##cacheSolve brings back the inverse of the cached matrix after calculting it with makeCacheMatrix
cacheSolve <- function(x, ...) {
  mi <- x$geti()
  if(!is.null(mi)){
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data,...)
  x$seti(mi)
  mi
}

