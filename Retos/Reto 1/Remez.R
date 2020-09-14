library(signal)
library(pracma)
library(Rmpfr)

e<-expression(exp(sin(x)-cos(x^2)))

evaluar<-function(coeficientes, x){
  res = c(0,0)
  grado = length(coeficientes) - 1
  res = coeficientes[grado+1]
  grado = grado - 1
  
  while(grado >= 0){
    if(coeficientes[grado + 1] != 0){
      res = res*x + coeficientes[grado+1]
    }
    else{
      res = res*x
    }
    grado = grado -1
  }
  return (res)
}

chebyshev <-function(a,b,numeros){
  resultado<-c()
  
  i=0
  while(i<length(numeros)){
    valor=(1/2)*(a+b)+(1/2)*(b-a)*cos((2*i-1)/(2*length(numeros))*pi)
    resultado<-append(resultado,valor)
    i=i+1
  }
  return(resultado)
}

bisectar<-function(a,b,maxiter,tol){
  pendiente=b/a
  e1<-expression(pendiente*x)
  g <- function(x) eval(e1)
  x = (a+b)/2
  i = 0
  
  while (g(x) != 0 && i<maxiter){
    
    error=abs(a-b)/2
    
    if(error >= tol)
      if (g(x)*g(a) < 0) b = x 
      else {a = x}
    else {break}  
      
      x=(a+b)/2
      
      i=i+1
  }
  return(x)
}

remez<-function(f,a,b,x,n){
  g <- function(x) eval(f)
  i=0
  ma<-matrix(1, nrow = n+2, ncol = n+2)
  i=1
  while(i<=n+2){
    j=2
    while(j<n+2){
      ma[i,j]<-x[i]^(j-1)
      j=j+1
    }
    ma[i,n+2]<-(-1)^(i-1)
    i=i+1
  }

  
  m<-1:(n+2)
  eva<-1:(n+2)
  z<-1:(n+1)
  acabar=FALSE
  while(!acabar){
    acabar=TRUE
    
    i=1
    while(i<=length(x)){
      eva[i]<-g(x[i])
      i=i+1
    }
    
    sol<-solve(ma,eva)
    i=1
    while (i<=length(x)){
      m[i]<-evaluar(sol,x[i])-g(x[i])
      i=i+1
    }
    
    
    positivo=FALSE
    if(m[1]<0){
      positivo=TRUE
    }
    i=2
    fallo=FALSE
    while(i<=length(m)){
      if(positivo){
        positivo=FALSE
        if(m[i]<0){
          fallo=TRUE;
        }
      }else{
        positivo=TRUE
        if(m[i]>0){
          fallo=TRUE
        }
      }
      i=i+1
    }
    if(fallo){
      break
    }
    for (k in m) {
      if(abs(k)>2^-90){
        acabar=FALSE
      }
    }
    
    if(!acabar){
      i=1
      while(i<=length(z)){
        z[i]=bisectar(x[i],x[i+1],50,1e-16)
        i=i+1
      }
      i=2
      x[1]<-bisectar(a,z[1],50,1e-16)
      while(i<=length(z)){
        x[i]<-bisectar(z[i-1],z[i],50,1e-16)
        i=i+1
      }
      x[length(x)]<-bisectar(z[length(z)],b,50,1e-16)
    }
  }
  
  if(acabar){
    return (sol)
  }
  return(null)
}

n=5
remez(e,-2^(-8),2^(-8),c(),n)
f<-function(x) eval(e)
taylor(f,0,n)

