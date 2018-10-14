## Put comments here that give an overall description of what your
## makeCacheMatrix for is the matrix object that will cache in inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
    
  }
  get = function()x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list (set=set,get=get, setinv=setinv,getinv=getinv)

}

## CacheSolve will compute the matrix inverse returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if(!is.null(inv)){
    message ("Receiving cached data")
    return (inv)
  }
  mat.data = x$get()
  inv = solve(mat.data,...)
  x$setinv(inv)
  return(inv)
}

##Test the inverse matrix cache
test = function (mat){
  temp = makeCacheMatrix(mat)
  start.time =Sys.time()
  CacheSolve(temp)
  dur=Sys.time() -start.time
  print(dur)
  
}


### Testing matrix for 100 rows and 100 coulmns
set.seed (1110000)
r = rnorm(1000000)
matl = matrix(r,nrow=1000,ncol=1000)
test(matl)
