makeCacheMatrix <- function(x = matrix()){
        inverse <- null
        set <- function(input) {
                x <<- input
                inverse<<-null
        }
        get <- function(x)
        setinverse<-function(solve) <- inverse <<- solve
        getinverse<-function(inverse)
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve<-function(x){
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("access available")
                return(inverse)
        }
        data<- x$get()
        inverse<-solve(data)
        x$setinverse(inverse)
}
