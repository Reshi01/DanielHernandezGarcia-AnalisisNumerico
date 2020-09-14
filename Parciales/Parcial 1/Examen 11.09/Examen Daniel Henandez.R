#Librerias
library(pracma)
library(Rmpfr)

#Definicion de funciones
g <- function(x)(x^2)-cos(x)
teorico1=mpfr("0.8241323123025224229609567857719911081427",128)
teorico2=mpfr("-0.82413231230252242296009567857719911081427",128)

#Recibe rango a, b
algoritmo <- function(a,b){
  tol=10^-16;
  cat("\n")
  x0=mpfr(a,128)
  x1=mpfr(b,128)
  anterior=0
  i=0
  error=mpfr("10",128)
  
  while(error>tol && i<100){
    x2=x1-g(x1)*((x1-x0)/(g(x1)-g(x0)))
    
    res=x2
  
    error= abs(res-anterior)
    
    cat(formatMpfr(res),"\n")
    x0=x1
    x1=x2
    anterior=res
    i=i+1
  }
  
}

algoritmo(-1,3)