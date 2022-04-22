makeCacheMatrix <- function(x = matrix()) {
QQQ <- NULL
  set <- function(y){
  x <<- y
  QQQ <<- NULL
}
get <- function() x
setM <- function(solve) QQQ <<- solve
getM <- function() QQQ
list(set = set, get = get, setM = setM, getM = getM)
}

cacheSolve <- function(x, ...) {
     QQQ <- x$getM()
    if(!is.null(QQQ)){
      message("getting cached data")
      return(QQQ)
    }
    m <- x$get()
    QQQ <- solve(m, ...)
    x$setM(QQQ)
    QQQ
}
x <- makeCacheMatrix(matrix(c(3,5,4,3,2,2,4,6,1), ncol=3, nrow=3))
cacheSolve(x)
x$get()
x$getM()
