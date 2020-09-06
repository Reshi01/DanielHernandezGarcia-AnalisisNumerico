#Metodo de Aitken con Newton
#Librerias
library(pracma)
library(Rmpfr)
options(digits = 22)

#Definicion de funciones
A <- (expression)((cos(2*x))^2 -x^2)
B <- (expression)(x*sin(x)-1)
C <- (expression)(x^3-2*x^2+(4/3)*x-8/27)
C2 <- (expression)((x-2/3)^3)

#Variables globales para graficar
raiz<-c()
iteraciones<-c()
valor_error<-c()
valor_error1<-c()

#Metodo para verificar si la funcion es par o impar 
#Recibe la funcion a evaluar y la raiz
parImpar <- function(e,x){
  g <- function(x) eval(e)
  if(g(x)==g(-x)){
    cat("Resultado:",formatMpfr(x),";",formatMpfr(-x),"\n")
  }else if(g(x)==-g(-x)){
    cat("Resultado:",formatMpfr(x),";",formatMpfr(-x),"\n")
  }else{
    cat("Resultado: ",formatMpfr(x),"\n")
  }
}

#Metodo de Newton
#recibe fucnion a evaluar, valor inicial, tolerancia y maximo de iteraciones
newton <- function(e,x0, tol,maxiter){
  cat("\n")
  ed <- D(e,'x')
  g <- function(x) eval(e)
  gd <- function(x) eval(ed)
  
  x=x0
  i = 1
  error = 1
  anterior = 0
  
  while(error>tol &&i < maxiter){
    x0=x
    x = x0- g(x0)/gd(x0)
    i = i+1
    error = abs(x-anterior)
    anterior=x
    cat("-Iteración",i,":","\n")
    cat("   * Newton es",formatMpfr(x),"\n")
    cat("\tError: ",formatMpfr(error),"\n\n")
  }
  parImpar(e,x)
  cat("\n")
}

#Metodo de Aitken Original
#Recibe la funcion a evaluar, valor inicial, tolerancia y maximo de iteraciones
mAitken <- function(e, x0, tol, maxiter){
  cat("\n")
  ed <- D(e,'x')
  g <- function(x) eval(e)
  gd <- function(x) eval(ed)
  
  indiceErr=0
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
    
    x1 = mpfr(x0- g(x0)/gd(x0),128)
    i = i+1
    
    x2 = mpfr(x1- g(x1)/gd(x1),128)
    i = i+1
    
    denominador = mpfr(x2-2*x1+x0,128)
    if(abs(denominador) < 10e-32){
      cat("El denominador es demasiado pequeño.\n\n")
      break
    }
    res = mpfr(x2 - (x2-x1)^2/denominador,128)
    error = abs(res-anterior)
    
    iteraciones <- append(iteraciones,j)
    raiz <- append(raiz,as.numeric(res))
    valor_error <- append(valor_error, as.numeric(error))
    if(indiceErr<=0){
      indiceErr=1
    }else{
      valor_error1<-append(valor_error1,as.numeric(error))
    }
    
    cat("-Iteración",j,":","\n")
    cat("     x0 es",formatMpfr(x0),"\n") 
    cat("     x1 es",formatMpfr(x1),"\n")  
    cat("     x2 es",formatMpfr(x2),"\n")  
    cat("   * Aitken es",formatMpfr(res),"\n")
    cat("\tError: ",formatMpfr(error),"\n\n")
    
    anterior = res
    j=j+1
  }
  valor_error1<-append(valor_error1,valor_error[length(valor_error)])

  plot(valor_error,raiz,"l")
  plot(iteraciones,raiz,"l")
  plot(valor_error,valor_error1,"l" )
  parImpar(e,res)
  cat("\n")
}

#Metodo de Aitken Modificado
#Recibe la funcion a evaluar, valor inicial, tolerancia y maximo de iteraciones
mAitkenMejorado <- function(e, x0, tol, maxiter){
  cat("\n")
  ed <- D(e,'x')
  g <- function(x) eval(e)
  gd <- function(x) eval(ed)
  
  indiceErr=0
  i = 1
  j = 1
  anterior = 0
  error = 1
  x0 = mpfr(x0, 128)
  
  while(error > tol && j<=maxiter){
    x1 = mpfr(x0- g(x0)/gd(x0),128)
    i = i+1
    
    x2 = mpfr(x1- g(x1)/gd(x1),128)
    i = i+1
    
    denominador = mpfr(x2-2*x1+x0,128)
    if(abs(denominador) < 10e-32){
      cat("El denominador es demasiado pequeño.\n\n")
      break
    }
    res = mpfr(x2 - (x2-x1)^2/denominador,128)
    error = abs(res-anterior)
    
    iteraciones <- append(iteraciones,j)
    raiz <- append(raiz,as.numeric(res))
    valor_error <- append(valor_error, as.numeric(error))
    if(indiceErr<=0){
      indiceErr=1
    }else{
      valor_error1<-append(valor_error1,as.numeric(error))
    }
    
    cat("-Iteración",j,":","\n")
    cat("     x0 es",formatMpfr(x0),"\n") 
    cat("     x1 es",formatMpfr(x1),"\n")  
    cat("     x2 es",formatMpfr(x2),"\n")  
    cat("   * Aitken es",formatMpfr(res),"\n")
    cat("\tError: ",formatMpfr(error),"\n\n")
    
    x0 = res
    anterior = res
    j=j+1
  }
  valor_error1<-append(valor_error1,valor_error[length(valor_error)])
  
  plot(valor_error,raiz,"l")
  plot(iteraciones,raiz,"l")
  plot(valor_error,valor_error1,"l" )
  parImpar(e,res)
  cat("\n")
}

#Definicion valor inicial
x0=mpfr("1",128)

#Llamada a las funciones
#newton(A,x0, 1e-32, 50)
#mAitken(A,x0, 1e-32, 50)
#mAitkenMejorado(A,x0, 1e-32, 50)
