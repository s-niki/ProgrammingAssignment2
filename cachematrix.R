## This is a programming assignment 2 in the R Programming course 
## on Coursera from the Johns Hopkins University

## Matrix inversion is a very computationally intensive for large
## size matrices.  Sometimes in loops the inverse of a matrix 
## needs to be computed only once. So to avoid recomputing 
## and to speed up the computation, we can calculate the
## result only once.  Then if trying to recompute the inverse 
## of unchanged matrix again, we'll have a pre-computed value,
## which will be returned instead.

## Function makeCacheMatrix creates a particular "matrix" 
## object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m_inv <- NULL           # initialization
    
    set <- function(y) {    # function is used for storing global variables of matrix and NULL
        x <<- y             # y is a passed from user matrix, stored in global x    
        m_inv <<- NULL
    }
    
    get <- function() x                     # a one line function
    set_inv <- function(inv) m_inv <<- inv  # store inv matrix in global environment
    get_inv <- function() m_inv             # return matrix stored in set_inv
    list(set = set, get = get,              # return list of 4 function
         set_inv = set_inv,
         get_inv = get_inv)

}


## cacheSolve function computes the inverse of the "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    m_inv <- x$get_inv()    # trying to get a value from the global environment
    if(!is.null(m_inv)) {   # checking if m_inf is not NULL. If it is not null
                            # then there's a stored value, which can be returned
    message("Returning cached data") # print message
    m_inv                    # return cached value
        
    }
    
    # if m_inv IS NULL, then computing the inverse matrix with solve() function
    
    data <- x$get()             # storing matrix
    m_inv <- solve(data, ...)   # computing inverse matrix with solve()
    x$set_inv(m_inv)            # storing the result in global environment
    m_inv                       # return inversed matrix
    
}


### To test if the functions are working correctly
#> m <- makeCacheMatrix()

# initailize with an easy matrix 
#> m$set( matrix(4:1, 2, 2))

# check the contents of variable /use of parens to retrive the matrix part of the object/
#> m$get()
#     [,1] [,2]
#[1,]    4    2
#[2,]    3    1

# compute the inverse and store in cache
#> cacheSolve(m)
#      [,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2

# check if the value is stored i.e. cached
#> cacheSolve(m)
#Returning cached data
#      [,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2

