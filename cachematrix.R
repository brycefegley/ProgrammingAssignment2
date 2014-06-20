## This function avoids repeat of the computationally intensive use of the 'solve' function to
## compute the inverse of a matrix for values of the matrix on which the 'solve' function has
## been previously called (the cached values).

## This first of two functions -- "make_cache_matrix" -- creates a special "matrix" which
## is really a list containing a function that:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the inverse of the matrix
## 4. Gets the inverse of the matrix

make_cache_matrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        set_solve <- function(solve) s <<- solve
        get_solve <- function() s
        list(set = set,
             get = get,
             set_solve = set_solve,
             get_solve = get_solve)
}

## This second of two functions solves the inverse of the special "matrix" created with the
## first function. However, it first checks to see if the inverse has already been solved.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the matrix and sets the value of the inverse in the cache via the 
## 'cache_solve' function.

cache_solve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$get_solve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$set_solve(s)
        s
}

## Example
A_matrix <- make_cache_matrix() ## Create empty matrix and plugin invertible structure
A_matrix$set(matrix(c(1, 4, 2, 2, 2, 3, 3, 1, 0), nrow = 3, ncol = 3))
A_matrix$get() ## print the matrix
cache_solve(A_matrix) ## solve the matrix inverse
cache_solve(A_matrix) ## test for the cache message
## create a different matrix
A_matrix$set(matrix(c(1, 0, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 1, 4, 1, 1), nrow = 4, ncol = 4))
A_matrix$get() ## check if new matrix shows up
cache_solve(A_matrix) ## solve the new matrix inverse
cache_solve(A_matrix) ## finally, check for cache message again
