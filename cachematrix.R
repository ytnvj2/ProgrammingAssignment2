makeCacheMatrix<-function(x=matrix()) #Martix creating function
  {
  inv<-NULL # initializing inverse to Null 
  set<-function(y) # set function to set the value of x
    {
    x<<-y # changing the value of x to y
    inv<<-NULL # Reinitializing inv to Null as matrix changed
  }
  get<-function() x # gets the matrix x and prints it
  setInverse<-function(inverse) inv<<-inverse #sets the value of inv to inverse
  getInverse<-function() inv #gets and prints the value of inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) # list of functions named as elements 
  
  
}
cacheSolve<-function(x,...) # calculates and caches the inverse of matrix create by makeCacheMatrix()
{
  inv<-x$getInverse() # gets the cached inverse value
  if(!is.null(inv)) # checks if cached value exists then returns it along with the message
    {
    message("Cached Data")
    return(inv)
  }
  data<-x$get() # sets data as matrix
  inv<-solve(data,...) # solve function run on data to find inverse and the result assigned to inv
  x$setInverse(inv) # sets the inverse of the matrix to inv
  inv # prints inv
}
