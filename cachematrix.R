## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(matrix){
                mat<<-matrix
                inv<<-NULL
        }
        get<-function()matrix(data, nrow = rows, ncol = cols)
        
        set.inverse<-function(setinv) inv<<-setinv
        get.inverse<-function() inv
        
        list(set = set, get = get, set.inverse=set.inverse,get.inverse=get.inverse)

}


## Write a short comment describing this function

cacheSolve <- function(cached.mat, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-cached.mat$get.inverse()
        
        if(!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        raw.mat<-cached.mat$get()
        inv<-solve(raw.mat,...)
        cached.mat$set.inverse(inv)
        inv
}
