## Caching is an important technique to speed up our work.
## It's a good way for optimizing.

## Now, we'll look at caching on computation of inverse matrix.
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix<-function(x=matrix()){
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x
    setInv<-function(solve) inv<<-solve
    getInv<-function() inv
    list(set=set, get=get, setInv=setInv,getInv=getInv)


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(x,...){
    inv<-x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setInv(inv)
    inv
}
         ##Testing##
n<-matrix(rnorm(9),3,3)       
## We create a 3x3 matrix, the elements are random normal variables
#           [,1]       [,2]       [,3]
# [1,] -0.2600403  1.3294046 -0.1370999
# [2,]  0.3523769  0.6016961 -2.0806852
# [3,] -0.3395655 -1.5610495  1.3489574
        
n1<-makeCacheMatrix(n)        
cacheSolve(n1)
## here, our inverse matrix
#           [,1]       [,2]       [,3]
# [1,] -2.4652202 -1.5979782 -2.7153349
# [2,]  0.2339231 -0.4020394 -0.5963469
# [3,] -0.3498540 -0.8675008 -0.6323113
        
#if we execute the cacheSolve function to the "same" matrix,
cacheSolve(n1)
#getting cached data    #we can see the "getting cached data" info. 
#           [,1]       [,2]       [,3]
#[1,] -2.4652202 -1.5979782 -2.7153349
#[2,]  0.2339231 -0.4020394 -0.5963469
#[3,] -0.3498540 -0.8675008 -0.6323113
        
