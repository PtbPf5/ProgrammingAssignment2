#  makeCacheMatrix script
#  Coursera R Programming
#  Programming Assignment 2: Caching the Inverse of a Matrix
#
#
# Function creates a special "matrix" object that can cache its inverse.
# Function computes the inverse of a square matrix using the solve() funtion.
#
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y)  # function that changes the vector stored in main func
        {
                x <<- y  #substitutes the vector x with y (input) in the main func
                m <<- NULL  # restors to null the value of m
        }
        get <- function() x # returns the vector x stored in main func
        setmatrix <- function(solve) m<<- solve  
        getmatrix <- function() m
        # list function stores for functions - set, get, setmatrix and get matrix
        # stores to an object that has all four functions
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
#
# Process explanation inspired by github/DanieleP/PA2-clarifying_instructions.
#
#
#
############################################################
############################################################
#
#  makeCacheMatrix script
#  Coursera R Programming
#  Programming Assignment 2: CacheSolve
#
#
# Function computes the inverse of special "matrix" returned from makeCacheMatrix function.
# If inverse has already been calculated, the the function will retrieve the inverse from the cache.
#
cacheSolve <- function(x=matrix(), ...) 
{
        m <- x$getmatrix() # verifying value of m previously stored is not NULL.
        if(!is.null(m))    # if m exists in memory, returns a message and value of m
        {
                message("getting cached data")
                return(m)  
        }
        matrix <- x$get()  # matrix call gets the matrix stored with makeCacheMatrix func
        m <- solve(matrix, ...)  # m calculates solve function
        x$setmatrix(m) # stores object
        m
}
#
# Process explanation inspired by github/DanieleP/PA2-clarifying_instructions.
#
#
