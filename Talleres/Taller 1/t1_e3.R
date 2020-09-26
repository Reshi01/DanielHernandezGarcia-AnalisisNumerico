library(Rmpfr)
options(digits = 22)
e1 <- (expression)(sin(x)-log(x+2))
e2 <- (expression)(-x*exp(x)+(x-x*exp(x)+3)*exp(x-x*exp(x)+3)-1)
e3 <- (expression)(exp(x)-x-1)
#1.a
interseccion1 <- function(e,x0,x1){
  f <- function(x) eval(e)
  error = 1
  i = 0
  while(error >= 10^(-16)){
    denominador = f(x1)-f(x0)
    if(denominador == 0){
      break
    }
    x2 = mpfr(x1 - ((f(x1)*(x1-x0))/denominador),128)
    cat("*Iteracion", i ,"resultado:",formatMpfr(x2),"\n")
    error = abs(x2 - x1)
    x0 = x1
    x1 = x2
    i = i + 1
  }
  val=mpfr(1.631443596968884822896061387697825694063,128)
}

#1.b
interseccion2 <- function(e,x0,x1){
  f <- function(x) eval(e)
  error = 1
  i = 0
  while(error >= 10^(-8)){
    denominador = f(x1)-f(x0)
    if(denominador == 0){
      break
    }
    x2 = mpfr(x1 - f(x1)*((x1-x0)/denominador),128)
    cat("*Iteracion", i ,"resultado:",formatMpfr(x2),"\n")
    error = abs(x2 - x1)
    x0 = x1
    x1 = x2
    i = i + 1
  }
  val=mpfr(1.631443596968884822896061387697825694063,128)
}

#2
newton <- function(e,x0, tol,maxiter){
  ed <- D(e,'x')
  g <- function(x) eval(e)
  gd <- function(x) eval(ed)

  x=mpfr(x0,128)
  i = 1
  error = 1
  anterior = 1

  while(error>tol && i<maxiter){
    x0=x
    x = x0- g(x0)/gd(x0)
    i = i+1
    error = abs(x-anterior)
    anterior=x
  }
  cat("Resultado: ",formatMpfr(x),'\n')
  return(x)
  
}

newtonGeneralizado<-function(x0,tol, maxiter){
  f <- function(x) eval(e2)
  ed<-D(e2,'x')
  fd<- function(x) eval(ed)
  
  error=1
  x=mpfr(x0,128)
  anterior=mpfr(x0,128)
  i=1
  while(error>tol && maxiter>i){
    x0=mpfr(x,128)
    nom=f(x0)*fd(x0)
    denom=(fd(x0))^2-f(x0)*fd(x0)
    x=x0-(nom/denom)
    error=abs(x-anterior)
    anterior=x
    i=i+1
  }
  cat("Resultado: ",formatMpfr(x), '\n')
}

resolverAB<-function(c,x){
  b=c+c*exp(c)-3
  a=c-b
  r=a+(x*a+b)*exp(x*a+b)
  cat("c: ",formatMpfr(c), '\n')
  cat("b: ",formatMpfr(b), '\n')
  cat("a: ",formatMpfr(a), '\n')
  cat("Resultado: ",formatMpfr(r), '\n')
}


#interseccion1(e1,0,-0.2)
#interseccion2(e1,0,-0.2)
#newton(e1,1,10e-8,100)
#resolverAB(newton(e2,1,10e-8,100),2)
#newtonGeneralizado(1,10e-16,100)