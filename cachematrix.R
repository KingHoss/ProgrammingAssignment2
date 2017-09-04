makeCatcheMatrix <- function(x = numeric()) {
  m <- matrix(NaN)                #initialize to NULL
  set <- function(y) {
    x <<- y
    m <<- matrix(NaN)
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  m <- x$getsolve()                 #calls getsolve above to return m
  if(m[1,1] != 'NaN') {             #if the returned m from getsolve above has value, use it
    message("getting cached data")
    return(m)
  }
  data <- x$get()                   #else return x from get above and store it in data
  m <- solve(data)                  #find the inverse of data
  x$setsolve(m)            #call to getsolve above to store and cache m for subsequent calls
  m                        #return m
}
