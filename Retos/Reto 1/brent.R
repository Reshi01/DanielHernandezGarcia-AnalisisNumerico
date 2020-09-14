options(digits = 22)
library(capn)
library(Rmpfr)
e1 <- (expression) (x^3 - 2*x^2 + (4*x/3)-(8/27))

e3 <- (expression)((cos(2*x))^2 -x^2)
e4 <- (expression)(x^3-2*x^2+(4/3)*x-8/27)
e5 <- (expression)(x*sin(x)-1)

f <- function(x) eval(e)
  
  
brent <-function(a,b,tol,e){
  f <- function(x) eval(e)
  valA = f(a)
  valB = f(b)
  cat(valA," ",valB)
   if(valA * valB >=0){
     return(-1)
   }
  
  if(abs(valA) < abs(valB)){
    aux = a
    a = b
    b = aux
    
  }
  c = mpfr(a,128)
  s = 0
  verificar = TRUE
  valS = 0
  rcon = 0
  anterior = a
  masAnterior = anterior

  #abs(b - a) > tol
  j = 1
  error = 20
  errorR=20
  cat("Iteración "," Valor \n")
  #error > tol
  while(error > tol ){
    valA = mpfr(f(a),128)
    valB = mpfr(f(b),128)
    valC = mpfr(f(c),128)
    if(valA != valC && valB != valC){
      s = mpfr((a*valB*valC)/((valA-valB)*(valA-valC))+(b*valA*valC)/((valB-valA)*(valB-valC))+(c*valA*valB)/((valC-valA)*(valC-valB)),128)
    }else{
      s = mpfr(b-valB*(b-a)/(valB-valA),128)
    }
    if((s <= (3*a+b)/4 || s >= b) || (verificar && abs(s-c) > abs(b-c)/2) || (!verificar && abs(s-c) > abs(c-d)/2) || (verificar && abs(b-c) < tol) || (!verificar && abs(c-d) < tol)){
      s = mpfr((a+b)/2,128)
      verificar = TRUE
    }else{
      verificar = FALSE
    }
    valS = mpfr(f(s),128)
    valA = mpfr(f(a),128)
    d = c
    c = mpfr(b,128)
    if(valA*valS < 0){
      b = s
    }else{
      a = s
    }
    valA = mpfr(f(a),128)
    valB = mpfr(f(b),128)
    if(abs(valA) < abs(valB)){
      aux = a
      a = b
      b = aux
    }
    
   
    
    error = abs(b-a)
    masAnterior = anterior
    anterior = c
    
    
    cat(j,"      ",formatMpfr(b),"\n")
    j = j+1
  }
  
  return(b)
}



cat("Raiz ",formatMpfr(brent(0,2,1e-16,e1)))


