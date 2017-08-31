
# create de matrix with cache options

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <-function(y){
                x<<-y
                inv<<-NULL
        }
        
        
        get     <- function () x
        setinv  <- function (m_inv) inv <<- m_inv 
        getinv  <- function () inv     
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        
        }




# Calculate inverse matrix with cache options

cacheSolve <- function(x,...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <-x$get()
        inv<- solve(data)
        x$setinv(inv)        
        inv
        
}

