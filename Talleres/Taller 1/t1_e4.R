library(pracma)
library(Rmpfr)
options(digits = 22)

e <- (expression)(3*sin(x)^3-1-4*sin(x)*cos(x))

#1
sucesion <- function(tol){
  n <- 1
  error <- 1
  anterior <- 0
  while(error > tol){
    x<- cos(1/n) 
    n <- n+1
    error <- abs(x-anterior)
    anterior <- x
    cat("En n =",n,"x =",x,"error =",error,"\n")
  }
}
sucesion(1e-8)

#2
sucesionAitken <- function(tol){
  anterior = 0
  error = 1
  n <- 1
  while(error > tol){
    x0 = cos(1/n)
    cat("-En n =",n,"x0 =",x0,"\n")  
    
    x1 = cos(1/(n+1))
    cat("-En n =",n,"x1 =",x1,"\n")  
    
    x2 = cos(1/(n+2))
    cat("-En n =",n,"x2 =",x2,"\n")  
    
    res = x2 - (x2-x1)^2/(x2-2*x1+x0)
    if(is.nan(res)){
      cat("El denominador es demasiado pequeno.\n")
      break
    }
    cat("*En Atiken n =", n ," x =",res,"\n")
    error = abs(res-anterior)
    cat("\tError: ",error,"\n")
    anterior = res
    n = n+1
  }
}
sucesionAitken(1e-8)

#3
mAitken <- function(e, x0, tol, maxiter){
  ed <- D(e,'x')
  g <- function(x) eval(e)
  gd <- function(x) eval(ed)
  
  i = 1
  j = 1
  anterior = 0
  error = 1
  x0 = mpfr(x0, 128)
  
  while(error > tol && j<=maxiter){
    if(j > 1){
      x0 = mpfr(x2- g(x2)/gd(x2),128)
      i = i+1
    }
    cat("-En iteración",j,"x0 es",formatMpfr(x0),"\n")
    
    x1 = mpfr(x0- g(x0)/gd(x0),128)
    i = i+1
    cat("-En iteración",j,"x1 es",formatMpfr(x1),"\n")  
    
    x2 = mpfr(x1- g(x1)/gd(x1),128)
    i = i+1
    cat("-En iteración",j,"x2 es",formatMpfr(x2),"\n")  
    
    res = mpfr(x2 - (x2-x1)^2/(x2-2*x1+x0),128)
    #x0 = res
    if(is.nan(res)){
      cat("El denominador es demasiado pequeno.\n")
      break
    }
    cat("*Iteracion", j ,"de Aitken es",formatMpfr(res),"\n")
    error = abs(res-anterior)
    cat("\tError: ",formatMpfr(error),"\n")
    anterior = res
    j=j+1
  }
}

mAitken(e,1, 1e-16, 50)