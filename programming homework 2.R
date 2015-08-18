#home week 3
# purpose of module: function for storing and retreiving the inverse of a matrix
# through the use of caching an object


#makeCacheMatrix
# defines the function that maintains the cache for the inverse of a matrix

makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    
    # set the inital cach to empty(null)
    set <- function(y){
      x <<- y
      m <<- NULL 
    }
    
    get <- function() x # return the passed value 
    
    #push inverse
    setInverse <- function(matrixInverse) m <<- matrixInverse
    # grab the cache
    getInverse <- function() m
    
    # decelrations for caching object
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

# cacheSolve
# purpose: solve the inverse of a invaritable matrix. this means any matrix that is a two by two. 
# this homework assignment that all inputs will be a the same hight and width(row length and column length EX 2X2,3X3,4X4 etc..).


cacheSolve <- function(myCache){
  m <- myCache$getInverse()
  
  if(!is.null(m)){ #do we have a entry for this cache?
    message("grabbing cached data")
    return(m)
  } 
  
  #if not cached lets add it to the cache and return the solved value
  
  m <- solve(myCache$get())
  myCache$setInverse(m)
  #return m
  return(m)
  
    
}

#quick unit test script i wrote up
# unit test data
mat4 <- matrix(c(1,1,1,3,4,3,3,3,4),nrow =3, ncol = 3)

unit_test <- function(matrix_input) {
  
  #no cache
  temp <- makeCacheMatrix(matrix_input)
  start.time = Sys.time()
  cacheSolve(temp)
  
  # store how long it took to calculate
  dur = Sys.time() - start.time
  print(dur)
  # now witch cache
  start.time = Sys.time()
  cacheSolve(temp)
  #how long did it take to grab it from the cache
  dur = Sys.time() - start.time
  print(dur)
}
