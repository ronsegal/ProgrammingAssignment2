## Put comments here that give an overall description of what your
## functions do

## These functions enable the results of an 'expensive' operation, matrix inversion
## to be effectively cached by assigning the result object from the first calculation to 
## a symbol in a containing environment rather than to the calculating function's local environment
## Consequently on subsequent calls to the calculating function if this cached result exists
## it is used rather than re-calculating the inversion.
## The approach makes use of the superassignment operator
## described here: http://bio-statistician.com/use-the-superassignment-operator.html

## Write a short comment describing this function

## This function creates a special matrix, returning a list of associated functions that:
## a. Returns the original matrix argument
## b. Caches the result of an inversion calculation in a variable in the parent environment
## c. Returns the cached inversion result from the parent environment (or NULL if it has not yet been created)

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL # initialise inversion result variable in makeCacheMatrix function environment

    getdata <- function() x # return the matrix object to be inverted
    setinvert <- function(invmat) inv <<- invmat # cache the inverted matrix result in parent environment
    getinvert <- function() inv # return NULL or the cached inverted matrix result from parent environment

    list(getdata = getdata, setinvert = setinvert, getinvert = getinvert) # return list of functions with same names

}


## Write a short comment describing this function

## Returns the result of a matrix inversion of an existing special matrix using a list of associated functions
## If the inversion has already been calculated the result is returned from cache
## If the inversion hasn't yet been calculated the function calculates the inversion
## and stores the result in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinvert() # assigns to NULL or cached inverted matrix
        if (!is.null(inv)) {
            message("Cached")
            return(inv)  # if not NULL returns cached result
        }
        data <- x$getdata() # else get original data
        inv <- solve(data) %*% data # invert the matrix
        x$setinvert(inv) # cache the result in parent environment
        inv
}

