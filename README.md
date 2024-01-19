# R-coding-
R coding

# TASK_1
#This function creates a special "matrix" object that can cache its inverse
cachematrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    
  }
    get<-function(){x}
    setInverse<-function(inverse){inv<<-inverse}
      getInverse<-function(){inv}
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
        
}


#TASK_2
#cachesolve retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        inv<-x$getInverse()## Return a matrix that is the inverse of 'x'
        if(!is.null(inv)){
          message("getting cached data")
          return (inv)
          
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
