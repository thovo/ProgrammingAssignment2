## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## I create a list to store the pair matrix and its inverse matrix 
## so whenever it need to compute the inverse matrix it will check in the list first
data_list <- list()
makeCacheMatrix <- function(x = matrix()) {
  ## This function is always return the inverse matrix and store it in the list
  ## in the order x, x_inverse
  x_inverse <- solve(x)
  data <- list(x,x_inverse)
  ## I don't know why but inside this function, it can not see the data_list
  ## I must put a global environment to let it know
  .GlobalEnv$data_list[[length(data_list)+1]] <- data
}

## Compare two matrices, get from  https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

## Write a short comment describing this function
## If will check the matrix in list, if the matrix exist, get from cache. If not
## call makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  data_list_length <- length(data_list)
  flag <- 0 # A flag to know x exist in data list
  x_inverse <- matrix()
  if(data_list_length > 0){
    for(i in 1:data_list_length)
    {
      ## Check for equal matrix
      if(matequal(data_list[[i]][[1]],x)){
        print("Found cached matrix")
        x_inverse = data_list[[i]][[2]]
        flag = 1
      }
    }
  }
  if(flag == 0){
    print("Solve the inverse matrix")
    x_inverse <- makeCacheMatrix(x)
  }
  x_inverse  
}
## Test
A <- matrix(c(1,2,3,0,1,4,5,6,0),ncol =3)
