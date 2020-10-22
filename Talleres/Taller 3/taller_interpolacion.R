library(polynom)
library(ggplot2)
library(Rmpfr)

#Ejercicio 1

#Conociendo la pendiente en el primer punto se, obtiene un punto antes del primero
#para aumentar el grado del polinomio de interpolaci???n. Utiliza interpolaci???n de Lagrange.

interpolar <- function(x,y,pendiente){
  deltaX = 1e-8
  primerX = (x[1]-deltaX)
  primerY = y[1] - pendiente*(deltaX)
  x = c(primerX, x)
  y = c(primerY, y)
  p = poly.calc(x,y)
  return(p)
}

x=c(0,1,2)
y=c(10,15,5)

p2 = poly.calc(x,y)
print("Polinomio de grado 2.")
print(p2)
plot(p2)
points(x,y, col="red")

p3 = interpolar(x,y,1)
print("Polinomio de grado 3.")
print(p3)
plot(p3)
points(x,y, col="red")

#Calcular derivada
e<-expression(10+x+9.75*x^2-5.75*x^3)
ed <- D(e,'x')
gd <- function(x) eval(ed)
print("Derivada en 0")
print(gd(0))

#Ejercicio 10

#Funci???n que interpola x=a a trav???s de Lagrange Modificado
lagmod <- function(x,y,a){
  n = length(x)
  fi = mpfr(1,128)
  for(i in 1:n){
    if(a == x[i]){
      return (y[i])
    }
    fi = fi * (a-x[i])
  }
  p =mpfr(0,128)
  for(i in 1:n){
    w = mpfr(1,128)
    for(j in 1:n){
      if(i!=j){
        w = w*(1/(x[i]-x[j]))
      }
    }
    p = p + (y[i]/(a-x[i]))*w
  }
  p = p * fi
  return(p)
}

x = c(0.2, 0.3, 0.4, 0.5)
y = c(1.2, 5.3, 9.4, 10.5)
print("Resultado con Lagrange Modificado")
print(lagmod(x, y, 0.35))

#Funci???n que interpola x=a a trav???s de Lagrange Baric???ntrico
lagbar <- function(x,y,a){
  n = length(x)
  numerador = mpfr(0,128)
  denominador = mpfr(0,128)
  for(i in 1:n){
    w = mpfr(1,128)
    for(j in 1:n){
      if(i!=j){
        w = w*(1/(x[i]-x[j]))
      }
    }
    if(a == x[i]){
      return (y[i])
    }
    numerador = numerador + (w/(a-x[i]))*y[i]
    denominador = denominador + w/(a-x[i])
  }
  p = numerador/denominador
  return(p)
}
x = c(0.2, 0.3, 0.4, 0.5)
y = c(1.2, 5.3, 9.4, 10.5)
print("Resultado con Lagrange Baricentrico")
print(lagbar(x, y, 0.35))
