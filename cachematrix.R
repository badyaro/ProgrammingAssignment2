
# makeCacheMatrix function creates the matrix that can store inversed value in the cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function( y ) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getInversed <- function() m
  setInversed <- function( inverse ) m <<- inverse
  list( set = set, get = get, getInversed = getInversed, setInversed = setInversed  )
}


# cacheSolve function returns inversed matrix.
# If inversed matrix exists in the cache it returns value from the cache
# otherwise it stores value in the cache
cacheSolve <- function(x, ...) {
  m <- x$getInversed()
  if( !is.null( m ) ) {
    #return value from cache
    message( "getting cached data" )
    return( m )
  }
  #else solve cache
  data <- x$get()
  m <- solve( data )
  x$setInversed( m )
  m
}
