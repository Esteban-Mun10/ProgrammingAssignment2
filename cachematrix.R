## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

####Esta primera función crea un objeto "matriz" especial que puede almacenar en caché su inverso.

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y){
        x <<- y
        a <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) a <<- inverse
    getInverse <- function() a 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function

### Esta segunda función calcula la inversa de la "matriz" especial creada por makeCacheMatrix arriba,
### si ya se ha calculado la inversa (y la matriz no ha cambiado), entonces debería recuperar la inversa de la caché

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    a <- x$getInverse()
    if(!is.null(a)){
        message("getting cached data")
        return(a)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(a)
    a
}
