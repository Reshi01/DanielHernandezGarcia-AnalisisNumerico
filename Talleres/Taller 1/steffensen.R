options(digits = 22)
library(Rmpfr)


e1 <- (expression)((x^2-cos(x)))

steffensen <- function(e,x0,tol,max){
  i=1

  x=0
  g <- function(x) eval(e)
  while(i<max){

    x = mpfr(x0 - (g(x0)^2)/(g(x0+g(x0))-g(x0)) ,128)
    cat(i," Valor",formatMpfr(x),"\n")
     if(abs(x-x0) < tol){
       return(x)
     }
    i = i+1
    x0=x
  }
}

steffensen(e1,2,1e-16,20)

