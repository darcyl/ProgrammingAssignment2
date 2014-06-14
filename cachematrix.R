##
## This R code, cachematrix.R, contains the functionality needed to compute 
## the inverse of a square matrix or just retrieve the inverse if it has been 
## computed previously.
##
## Example of how these functions could be used/tested
## Step 1 - create/define the vector of functions (MatrixFunctionsVector) which 
##          can be used to manage the matrix and its inverse.  No results returned
##          but should see MatrixFunctionsVector now listed as a value and is a list of 4.
##
##          >MatrixFunctionsVector <- makeCacheMatrix()
##
## Step 2 - define a 2x2 matrix comprised of values 1 thru 4 and set/save it to
##          the global matrix variable using the setMatrix() function defined within 
##          the MatrixFunctionsVector vector.  This function also establishes the
##          global InverseMatrix variable and initializes it to null (inverse 
##          corresponding to Matrix global variable is not yet computed). No results
## 
##          >MatrixFunctionsVector$setMatrix(matrix(1:4,2,2))
##
## Step 3 - retrieve and return the contents of the Matrix variable using the
##          getMatrix() function defined within the MatrixFunctionsVector vector.
##          At any rate the matrix created in step 2 should be returned.
## 
##          > MatrixFunctionsVector$getMatrix()
##               [,1] [,2]
##          [1,]    1    3
##          [2,]    2    4
##
## Step 4 - passing the vector of functions to the cacheSolve() function
##          should now return the inverse of the matrix stored in the global 'Matrix'
##          variable as well as caches the inverse in the 'InverseMatrix' global 
##          variable if does not already exist or is set.
## 
##          > cacheSolve(MatrixFunctionsVector)
##          [,1] [,2]
##          [1,]   -2  1.5
##          [2,]    1 -0.5
##
## Function makeCacheMatrix() creates a list comprised of 4 atomic level functions 
## that can be used to manage caching inverse of a matrix and reuse of a previously 
## computed inverse of a matrix.
##
## Note: the name of this function is confusing but was required to be named as such
## for this assignment.  This function is not actually caching a matrix but 
## defining a set of functions for manging a matrix and its inverse.  A better name
## would have been defineMatrixFunctions.
##
makeCacheMatrix <- function(Matrix = matrix()) {
        InverseMatrix<-NULL
        ##
        ## Function setMatrix() is used to store a matrix in the variable
        ## 'Matrix' and will initialize the variable storing the inverse
        ## of the matrix, i.e. 'InverseMatrix', to NULL.
        ##
        setMatrix <- function(y) {
                message("setMatrix() called") 
                if (!identical(Matrix,y))
                {    
                        ##message("new matrix")
                        Matrix <<- y
                        InverseMatrix <<- NULL
                }    
        }
        
        ##
        ## Function getMatrix() retrieves the contents of the global variable 'Matrix'
        ##
        getMatrix  <- function()  {
                ##message("getMatrix() called")
                Matrix
        }
        ##
        ## Function setInverse() save the contents of the argument passed to it into
        ## the global variable 'InverseMatrix'.  
        ##
        setInverse <- function(Inverse) {
                ##message("setInverse() called")
                InverseMatrix <<- Inverse
        }        
        
        ##
        ## Function getInverse() retrieves the contents of from global variable 
        ## 'InverseMatrix' and returns it to the calling function.  
        ##
        getInverse <- function() {
                ##message("getInverse() called")
                InverseMatrix
        }
                
        ##
        ## Create a list containing the 4 functions defined above.
        ##        
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

##
## Function cacheSolve() returns the inverse matrix for the matrix stored in the 
## global variable 'Matrix'.
##
## Arguments: 
##      x - is a vector of functions to set and retrieve a matrix and its inverse 
##          matrix from/to global variables.  
##
cacheSolve <- function(x, ...) {
        Inverse<-x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        Inverse <- solve(x$getMatrix())
        x$setInverse(Inverse)
        Inverse
}
