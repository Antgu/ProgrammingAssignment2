## This file contains two functions, one for creating a list containing a matrix and one 
## for calculating the inverse of the matrix in the list. The neat thing is that the inverse 
## gets stored in the list and that the function for calculating the inverse actually
## checks if the inverse has already been calculated and uses the stored value if it has. This
## saves computing effort for large matrixes.

## This function creates the "special matrix list". 
## Example: mat<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
## The return value (stored in mat) is a list, where the matrix can be reached with function 
## get() stored in the list.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
            x<<-y
            inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## The cacheSolve function calculates the inverse of a matrix in an object (list) created 
## by the makeCacheMatrix function. It checks if the inverse has already been calculated 
## and returns the stored inverse if it has (using the getinv() function that is contained 
## in the list). If the inverse hasn't already been calculated it calculates it and then 
## stores the result in the list by using the setinv() function.

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setinv(inv)
        inv
}
