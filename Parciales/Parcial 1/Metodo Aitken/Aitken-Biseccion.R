#Metodo de Aitken con Biseccion
#Librerias
library(pracma)
library(Rmpfr)
options(digits = 22)

#Definicion de funciones
A <- (expression)((cos(2*x))^2 -x^2)
B <- (expression)(x*sin(x)-1)
C <- (expression)(x^3-2*x^2+(4/3)*x-8/27)
C2 <- (expression)((x-2/3)^3)

#Variables globales
raiz<-c()
iteraciones<-c()
valor_error<-c()
valor_error1<-c()

#Metodo para verificar si la funcion es par o impar 
#Recibe la funcion a evaluar y la raiz
parImpar <- function(e,x){
  g <- function(x) eval(e)
  if(g(x)==g(-x)){
    cat("Resultado",formatMpfr(x),";",formatMpfr(-x),"\n")
  }else if(g(x)==-g(-x)){
    cat("Resultado I:",formatMpfr(x),";",formatMpfr(-x),"\n")
  }else{
    cat("Resultado: ",formatMpfr(x),"\n")
  }
}

#Metodo de Biseccion
#recibe la funcion a evaluar, rango [a,b], tolerancia y mximo de iteraciones
biseccion <- function(e,a,b,tol,maxiter) 
{
  cat("\n")
  g <- function(x) eval(e)
  x <- (a+b)/2
  i <- 0
  
  while (g(x) != 0 && i<maxiter) 
  {   
    
    error<-abs(a-b)/2
    
    if(error >= tol)
      if (g(x)*g(a) < 0) b <- x 
      else {a <- x}
      else {break}  
      
      x<-(a+b)/2
      
      i<-i+1
      
      cat("-Iteración",i,":","\n")
      cat("   * Newton es",formatMpfr(x),"\n")
      cat("\tError: ",formatMpfr(error),"\n\n")
  }
  parImpar(e,x)
  cat("\n")
}

#Metodo de Aitken Original 
#Recibe la funcion a evaluar, rango [a,b], tolerancia y mximo de iteraciones
mAitken <- function(e,a,b,tol,maxiter){
  cat("\n")
  indiceErr=0
  g <- function(x) eval(e)
  if(g(a) * g(b) > 0){
    cat("Selecciona otros a y b. \n")
    return()
  }

  i=0
  error = 1
  anterior = b
  while(error>tol && i<maxiter){
    x0 =(a+b)/2
    gx = g(x0)
    if(gx == 0){
      break
    }
    if(gx * g(a) <0){
      b=x0
    }else{
      a=x0
    }
    
    x1 = (a+b)/2
    gx = g(x1)
    if(gx == 0){
      break
    }
    if(gx * g(a) <0){
      b=x1
    }else{
      a=x1
    }
    
    x2 = (a+b)/2
    gx = g(x2)
    if(gx == 0){
      break
    }
    if(gx * g(a) <0){
      b=x2
    }else{
      a=x2
    }
    
    denominador = mpfr(x2-2*x1+x0,128)
    if(abs(denominador) < 10e-32){
      cat("El denominador es demasiado pequeño.\n\n")
      break
    }
    res = mpfr(x2 - (x2-x1)^2/denominador,128)
    i = i+1
    error = abs(res-anterior)
    anterior = res
    
    iteraciones <- append(iteraciones,i)
    raiz <- append(raiz,as.numeric(res))
    valor_error <- append(valor_error, as.numeric(error))
    if(indiceErr<=0){
      indiceErr=1
    }else{
      valor_error1<-append(valor_error1,as.numeric(error))
    }

    cat("-Iteración",i,":","\n")
    cat("     x0 es",formatMpfr(x0),"\n") 
    cat("     x1 es",formatMpfr(x1),"\n")  
    cat("     x2 es",formatMpfr(x2),"\n")  
    cat("   * Aitken es",formatMpfr(res),"\n")
    cat("\tError: ",formatMpfr(error),"\n\n")
  }
  valor_error1<-append(valor_error1,valor_error[length(valor_error)])
  
  plot(valor_error,raiz,"l")
  plot(iteraciones,raiz,"l")
  plot(valor_error,valor_error1,"l" )
  
  parImpar(e,res)
  cat("\n")
}

#Metodo de Aitken Original 
#Recibe la funcion a evaluar, rango [a,b], tolerancia y mximo de iteraciones
mAitkenMejorado <- function(e,a,b,tol,maxiter){
  cat("\n")
  g <- function(x) eval(e)
  if(g(a) * g(b) > 0){
    cat("Selecciona otros a y b. \n\n")
    return()
  }
  
  indiceErr=0
  i=0
  error = 1
  anterior = b
  x0 = (a+b)/2
  while(error>tol && i<maxiter){
    gx = g(x0)
    if(gx == 0){
      break
    }
    if(gx * g(a) <0){
      b=x0
    }else{
      a=x0
    }
    
    x1 = (a+b)/2
    gx = g(x1)
    if(gx == 0){
      break
    }
    if(gx * g(a) <0){
      b=x1
    }else{
      a=x1
    }
    
    x2 = (a+b)/2
    gx = g(x2)
    if(gx == 0){
      break
    }
    if(gx * g(a) <0){
      b=x2
    }else{
      a=x2
    }
    
    denominador = mpfr(x2-2*x1+x0,128)
    if(abs(denominador) < 10e-32){
      cat("El denominador es demasiado pequeño.\n\n")
      break
    }
    res = mpfr(x2 - (x2-x1)^2/denominador,128)
    i = i+1
    error = abs(res-anterior)
    x0 = res
    anterior = res
  
    iteraciones <- append(iteraciones,i)
    raiz <- append(raiz,as.numeric(res))
    valor_error <- append(valor_error, as.numeric(error))
    if(indiceErr<=0){
      indiceErr=1
    }else{
      valor_error1<-append(valor_error1,as.numeric(error))
    }
    
    cat("-Iteración",i,":","\n")
    cat("     x0 es",formatMpfr(x0),"\n") 
    cat("     x1 es",formatMpfr(x1),"\n")  
    cat("     x2 es",formatMpfr(x2),"\n")  
    cat("   * Aitken es",formatMpfr(res),"\n")
    cat("\tError: ",formatMpfr(error),"\n\n")
  }
  valor_error1<-append(valor_error1,valor_error[length(valor_error)])
  
  plot(valor_error,raiz,"l")
  plot(iteraciones,raiz,"l")
  plot(valor_error,valor_error1,"l" )
  
  parImpar(e,res)
  cat("\n")
}

#Definicion del rango [a,b]
a=mpfr("0",128)
b=mpfr("2",128)

#Llamada a las funciones
#biseccion(B,a,b, 1e-32,200)
#mAitken(B,a,b, 1e-32,50)
#mAitkenMejorado(C,a,b, 1e-32,50)


