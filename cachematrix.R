#Following functions are used to define a special "matrix" object ("makeCacheMatrix")
#and calculate inverse of defined special "matrix" ("cacheSolve")



#In following function special "matrix" object is defined with getter and setter.
#which returns it's inversed version using getter (getinverse) and setter (setinverse) 

#If matrix inverse has already been calculated, 
#getter (getinverse) gets the inverse matrix from the cache and returns it,
#else if matrix inverse not calculated, 
#it will be calculated and will be set into cache using setter (setinverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL;
  
  set<-function(y){
    x<<-y
    m <<- NULL
  }
  
  get<-function(){
    x
  }
  
  setinverse <- function(inverse) {
    m <<- inverse
  }
  
  getinverse <- function(){
    m
  } 
    
  list(set = set, 
       get = get,
       
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
#First I check if matrix invers exist in chache,
#if exist then I return matrix invers from cache
#else calculate imatrix inverse using solve(matrix) function
#and put matrix inverse to cache using setmatrix function

cacheSolve <- function(x, ...) {
  
  m<-x$getinverse()
  if(!is.null(m)){
    m
  } else {
    matrix <- x$get()
    m <- solve(matrix)
    x$setmatrix(m)
    m
  }
}
