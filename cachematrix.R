
## this function is used By a function  called cacheSolve to create a cache 
#of a matrix. 
#It returns a list

makeCacheMatrix <- function(x = matrix()) # a matrix gets into variable x
{
        InverseCache <- NULL 
        set <- function(y) {    # the set function passes a matrix into x
                x <<- y 
                InverseCache <<- NULL # not inverse matrix yet
        } 
        get <- function() x # gets the source matrix
        set_inverse <- function(solve) InverseCache<<- solve # this function
        # receive the inverse of a matrix from another function and stores it 
        #as InverseCache
        get_inverse <- function() InverseCache # returns the cached inverse matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)# creates the Cache 
}


## this function uses solve() to create the inverse matrix

cacheSolve <- function(x) # it receives a matrix created with makeCacheMatrix()
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()# if the inverse in cached, returns the cached inverse
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        message("creating new cache with the matrix:")
        data <- x$get()# if not, it creates the inverse matrix and stores it 
        print(data)# in the cache
        m <- solve(data)
        x$set_inverse(m)
        m #Returns the inverse
}

createMatrix<- function()
{
        # R program to find inverse of a Matrix, 
        #source https://www.geeksforgeeks.org/inverse-of-matrix-in-r/
        
        # Create 3 different vectors
        # using combine method.
        a1 <- c(3, 2, 5)
        a2 <- c(2, 3, 2)
        a3 <- c(5, 2, 4)
        
        # bind the three vectors into a matrix 
        # using rbind() which is basically
        # row-wise binding.
        A <- rbind(a1, a2, a3)
}



