library(akima)
library(akima)
library('plot.matrix')
library(pracma)
library(ggplot2)


#Intento 1:
#Teniendo la lista de todos los puntos en x y, se inteno tratar la informacion como una imagen.
#Para ello es necesario tener otro vecor z que representa "la intensidad del bit"
#Como solo se esta evaluando el contorno, al tratar la informacion como imagen,
#todo el pato queda de la misma intensiada.
x=c(0.5,0.9,1,1.3,1.6,1.8,1.8,1.95,2.2,2.25,2.5,2.55,2.9,3,3.15,3.5,3.8,4,4,4.7,5,5.4,5.5,5.5,5.55,
    5.65,5.7,5.8,5.85,6,6,6,6.2,6.2,6.3,6.35,6.5,6.55,6.6,6.7,6.7,6.8,6.95,7,7.4,7.4,7.8,7.8,8.1,8.4,8.4,
    8.65,8.85,8.9,9.05,9.25,9.25,9.4,9.55,9.6,9.65,9.8,9.9,10,10.1,10.15,10.25,10.4,10.5,10.55,10.65,11,
    11,11.35,11.45,12,12,12.45,12.55,13,13,13.4,13.4,13.75,13.75,14,14.15,14.3,14.7,14.7,15,15.25,15.45,
    15.85,16,16.35,16.4,17)
y=c(0.25,0,0.55,0,0.8,0.1,0.9,1.15,0.2,1.5,0.25,1.8,2.1,0.15,2.15,2.15,2.1,0.1,2,1.7,0.05,1.35,-0.25,
    -8.2,1.3,-8.3,-7.9,1.35,-7.4,-6.85,-0.8,-8.35,1.4,-6,-8.3,-5,-4,1.45,-3,-2,-8.2,-1.2,1.5,-8,1.55,-7.7,
    1.6,-7.35,-7.1,1.6,-6.7,-6.4,-6.05,1.6,-5.7,1.6,-5.35,-5,-4.6,1.55,-4.35,-3.95,-3.65,-3.3,-2.9,1.45,-2.6,
    -2.4,-2.1,-1.95,1.4,1.3,-2,-2,1.2,0.95,-2,0.75,-1.85,0.55,-1.65,0.3,-1.4,0.15,-1.4,-0.1,-1.4,-0.3,-0.45,
    -1.4,-0.55,-1.3,-0.6,-1.2,-0.7,-0.8,-1.15,-1)
z=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
xs <- seq(0.5, 17, by = 0.5)
r=(interp(x,y,z))
image(r)


#Intento 2
#En esta parte se dividio las parejas xy, de tal manera que  para cada x solo exista una imagen
#Sup es el contorno superior del pato
#Inf 1 es la parte inferior de la cabeza del pato
#Inf 2 es la parte interna del ala del pato
#Inf 3 es la parte externa del ala junto con la parte trasera inferior del pato
#Para graficar las funciones resultantes es necesario limitar el rango de cada funcion, especialmente la funcion
#correspondiente a la parte interna del ala, debido a que por su inclinacion, la funcion resultante 
#crece muy rapidamente
xs <- seq(0.5, 17, by = 0.5)
xsup=c(0.5,1,1.6,1.8,1.95,2.25,2.55,2.9,3.15,3.5,3.8,4,4.7,5.4,5.55,5.8,6.2,6.55,6.95,7.4,7.8,8.4,8.9,9.25,
       9.6,10.15,10.65,11,11.45,12,12.45,13,13.4,13.75,14,14.3,14.7,15,15.45,16,16.35,17)
ysup=c(0.25,0.55,0.8,0.9,1.15,1.5,1.8,2.1,2.15,2.15,2.1,2,1.7,1.35,1.3,1.35,1.4,1.45,1.5,1.55,1.6,1.6,1.6,
       1.6,1.55,1.45,1.4,1.3,1.2,0.95,0.75,0.55,0.3,0.15,-0.1,-0.3,-0.45,-0.55,-0.6,-0.7,-0.8,-1)
xinf1=c(0.5,0.9,1.3,1.8,2.2,2.5,3,4,5,5.5,6.8)
yinf1=c(0.25,0,0,0.1,0.2,0.25,0.15,0.1,0.05,-0.25,-1.2)
xinf2=c(5.5,5.7,5.85,6,6.2,6.35,6.5,6.6,6.7,6.8)
yinf2=c(-8.2,-7.9,-7.4,-6.85,-6,-5,-4,-3,-2,-1.2)
xinf3=c(5.65,6,6.3,6.7,7,7.4,7.8,8.1,8.4,8.65,8.85,9.05,9.25,9.4,9.55,9.65,9.8,9.9,10,10.1,10.25,10.4,10.5,
        10.55,11,11.35,12,12.55,13,13.4,13.75,14.15,14.7,15.25,15.85,16.4,17)
yinf3=c(-8.3,-8.35,-8.3,-8.2,-8,-7.7,-7.35,-7.1,-6.7,-6.4,-6.05,-5.7,-5.35,-5,-4.6,-4.35,-3.95,-3.65,-3.3,
        -2.9,-2.6,-2.4,-2.1,-1.95,-2,-2,-2,-1.85,-1.65,-1.4,-1.4,-1.4,-1.4,-1.3,-1.2,-1.15,-1)


f<-cubicspline(xsup, ysup)
sup <- function(xs) ppval(f, xs)


f1<-cubicspline(xinf1, yinf1)
inf1 <- function(xs) ppval(f1, xs)


f2<-cubicspline(xinf2, yinf2)
inf2 <- function(xs) ppval(f2, xs)

f3<-cubicspline(xinf3, yinf3)
inf3 <- function(xs) ppval(f3, xs)


p <- ggplot(data = data.frame(x = c(0.5,17)), mapping = aes(x = x))
p <- p +
  stat_function(fun=sup,aes(colour="Sup")) +
  ylim(-8.5,2.5)
p<-p + stat_function(fun=inf1, aes(colour="Inf1"), xlim=c(0.5,6.8))
p<-p + stat_function(fun=inf2, aes(colour="Inf2"), xlim=c(5.45,6.8))
p<-p + stat_function(fun=inf3, aes(colour="Inf3"), xlim=c(5.45,17))
print(p)



#Intenti 3
#Este intento esta basado en el intento 1 pero agregando difrentes intensidades, lo que 
#permite generar  "puntos en blanco". El problema resultante es que tambien se generan 
#espacios en blanco en el contorno.
x=c(0.5,0.9,1,1.3,1.6,1.8,1.8,1.95,2.2,2.25,2.5,2.55,2.9,3,3.15,3.5,3.8,4,4,4.7,5,5.4,5.5,5.5,5.55,
    5.65,5.7,5.8,5.85,6,6,6,6.2,6.2,6.3,6.35,6.5,6.55,6.6,6.7,6.7,6.8,6.95,7,7.4,7.4,7.8,7.8,8.1,8.4,8.4,
    8.65,8.85,8.9,9.05,9.25,9.25,9.4,9.55,9.6,9.65,9.8,9.9,10,10.1,10.15,10.25,10.4,10.5,10.55,10.65,11,
    11,11.35,11.45,12,12,12.45,12.55,13,13,13.4,13.4,13.75,13.75,14,14.15,14.3,14.7,14.7,15,15.25,15.45,
    15.85,16,16.35,16.4,17)

y=c(0.25,0,0.55,0,0.8,0.1,0.9,1.15,0.2,1.5,0.25,1.8,2.1,0.15,2.15,2.15,2.1,0.1,2,1.7,0.05,1.35,-0.25,
    -8.2,1.3,-8.3,-7.9,1.35,-7.4,-6.85,-0.8,-8.35,1.4,-6,-8.3,-5,-4,1.45,-3,-2,-8.2,-1.2,1.5,-8,1.55,-7.7,
    1.6,-7.35,-7.1,1.6,-6.7,-6.4,-6.05,1.6,-5.7,1.6,-5.35,-5,-4.6,1.55,-4.35,-3.95,-3.65,-3.3,-2.9,1.45,-2.6,
    -2.4,-2.1,-1.95,1.4,1.3,-2,-2,1.2,0.95,-2,0.75,-1.85,0.55,-1.65,0.3,-1.4,0.15,-1.4,-0.1,-1.4,-0.3,-0.45,
    -1.4,-0.55,-1.3,-0.6,-1.2,-0.7,-0.8,-1.15,-1)

z= rep(1, times = 98)

px = seq(0,20,0.5)
py = seq(-10,10,0.5)

for(i in px){
  for (j in py){
    duplicado = FALSE
    for(a in 1:length(x)){
      if(x[a] == i){
        if(y[a] == j){
          duplicado = TRUE
          break
        }
      }
    }
    if(!duplicado){
      x = c(x, i)
      y = c(y, j)
      z = c(z, 0)
    }
  }
}

res = interp(x,y,z,nx=80,ny=80)

image(res$x,res$y,res$z)


