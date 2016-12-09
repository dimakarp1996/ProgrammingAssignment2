makeCacheMatrix <- function(x=matrix()) 
{
  m<-NULL
  if (as.numeric(as.integer(sqrt(length(x))))!=sqrt(length(x)))
  {
    print("Error")
    warning()
  }
  set<-function(y)#here we set a variable
  {
    x<<-y
    m<<-NULL
  }
  toCache<-function(obj)#here we move solution to the cache
  {m<<-obj}
  getx<-function(){x}#here we read a variable
  getInvert<-function(){m}#here we read a solution from the cache
  list(set=set,toCache=toCache,getx=getx,getInvert=getInvert)#function returns a list of methods
}
cacheSolve<-function(y,...)
{
  m<-y$getInvert()#we check if the matrix is already inverted
  if(!is.null(m))
  {
    print("Data are cached")
    m#if yes - we do nothing
  }
  else
  {
	  data<-y$getx()#if no - we get variable
	  m<-solve(data)#invert
	  y$toCache(m)#cache
	  m#and return
  }
}