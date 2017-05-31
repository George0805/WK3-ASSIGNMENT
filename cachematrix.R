## This "cachematrix" function consisits of two functions
## works through the following steps to calculate and cache
## the inverse of the matrix being called.

## Step 1: Initialize two objects--x and inv.
## Step 2: Define the function for objects of makeCacheMatrix().
## Step 3: Create a new object by returining a list().
## Step 4: Computes the inverse of the matrix bing called in the
## first function.

## The first function creates a matrix that can cache its inverse
## and return a list containing the elements needed for step 4 in
## the second function.
makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){##set the matrix
            x<<-y
            inv<<-NULL
      }
      ##get matrix
      get<-function() x
      ##set inverse
      setinv <- function(solve) x <<-solve
      ##get inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      if(!is.null(inv)){
            return(inv)
      }
      else{
            data<-x$get()
            inv<-solve(data, ...)
            x$setinv(inv)
            inv
      }
}
