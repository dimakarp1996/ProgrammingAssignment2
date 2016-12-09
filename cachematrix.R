makeCacheMatrix <- function(x=matrix()) 
{
  m<-NULL
  if (as.numeric(as.integer(sqrt(length(x))))!=sqrt(length(x)))
  {
    print("Error")
    warning()
  }
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  toCache<-function(obj)
  {m<<-obj}
  getx<-function(){x}
  getInvert<-function(){m}
  list(set=set,toCache=toCache,getx=getx,getInvert=getInvert)
}
cacheSolve<-function(y,...)
{
  m<-y$getInvert()
  if(!is.null(m))
  {
    print("Data are cached")
    m
  }
  data<-y$getx()
  m<-solve(data)
  y$toCache(m)
  m
}