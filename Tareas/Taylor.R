f=expression(sqrt(c))

aproximacionTaylor <- function(c,v,n){
  i<-0
  res<-0
  while(i<=n){
    res<-res+((eval(f)/factorial(i))*(v-c)^i)
    f<-D(f,'c')
    i<-i+1
  }
  cat("T=",res,"\n")
}