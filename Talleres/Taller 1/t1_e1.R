library(Rmpfr)
options(digits = 22)

#N.1
f <- expression(((x^51)-1)/(x-1))
evaluarPolinomio <- function(){
  x=mpfr('1.00000000001',128)
  res = mpfr('1',128)
  grado = 50
  while(grado >0){
    res=res*x+1
    grado=grado-1
  }
  cat("Evaluacion polinomica: ",formatMpfr(res), '\n')
  g=function(x) eval(f)
  a=g(x)
  cat("Evaluacion equivalente: ",formatMpfr(a), '\n')
  cat("Error: ",formatMpfr(a-res),"\n")
}

#2
decimalBinario<-function(x){
  res<-c()
  i=0
  val=1
  while(i<100 && val!=0){
    val=2*x
    if(val>=1){
      res<-append(res,1)
      x=val-1
    }else{
      res<-append(res,0)
      x=val
    }
    i=i+1
  }
  
  i=1
  cat("Parte decimal: ")
  while(i<=length(res)){
    cat(res[i])
    i=i+1
  }
  cat('\n')
}
enteroBinario<-function(x){
  if(!is.integer(x)){
    residuo=x %% 1
    x=x-residuo
  }
  res<-c()
  mod=0;
  val=0;
  while(x/2!=0){
    mod=x%%2
    val=(x-mod)/2
    res<-append(res,mod)
    x=val
  }
  
  i=length(res)
  cat("Parte entera: ")
  while(i>0){
    cat(res[i])
    i=i-1
  }
  cat('\n')
  decimalBinario(residuo)
}

binarioEntero<-function(coeficientes){
  x=2
  res = mpfr(0,128)
  res = coeficientes[1]
  grado = 2
  
  while(grado <= length(coeficientes)){
    if(coeficientes[grado] != 0){
      res = res*x + coeficientes[grado]
    }
    else{
      res[1] = res[1]*x
    }
    grado = grado +1
  }
  return (res)
}
binarioDecimal<-function(coeficientes){
  x=2
  res = mpfr(0,128)
  grado = length(coeficientes)
  res=coeficientes[grado]
  grado=grado-1
  while(grado > 0){
    if(coeficientes[grado] != 0){
      res = res*(1/x) + coeficientes[grado]
    }
    else{
      res = res*(1/x)
    }
    grado = grado -1
  }
  res=res*(1/x)
  return (res)
}
abc <- function(a,b,c){
  b=-b
  nom1=b+sqrt((b^2)-4*a*c)
  nom2=b-sqrt((b^2)-4*a*c)
  den=2*a
  res=nom1/den
  cat("Raiz 1: ",res,"\n")
  res=nom2/den
  cat("Raiz 2: ",res,"\n")
}
abcMejorado<- function(a,b,c){
  nom=4*a*c
  den=2*a*(-b-sqrt((b^2)-4*a*c))
  den2=2*a*(-b+sqrt((b^2)-4*a*c))
  if(den==0){
    nom1=b-sqrt((b^2)-4*a*c)
    den=2*a
    res=nom1/den
  }else{
    res=nom/den
  }
  cat("Raiz 1: ",res,"\n")
  if(den2==0){
    nom2=-b-sqrt((b^2)-4*a*c)
    den=2*a
    res=nom2/den
  }else{
    res=nom/den2
  }
    cat("Raiz 2: ",res,"\n")
}
A <- (expression)(((x^2)+(9^12)*x-3))
#evaluarPolinomio()
#enteroBinario(3.14159265358979)
#binarioEntero(c(1,1,1))
#binarioDecimal(c(0,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1))
#abc(1,9^12,-3)
#abcMejorado(1,9^12,-3)