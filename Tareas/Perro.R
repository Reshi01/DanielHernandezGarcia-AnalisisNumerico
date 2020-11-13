library(akima)
library(akima)
library('plot.matrix')
library(pracma)
library(ggplot2)

#En esta parte se dividio las parejas xy, de tal manera que  para cada x solo exista una imagen
#Sup es el contorno superior del perro
#Inf 1 es la parte inferior de la cola del perro
#Inf 2 es el talon del perro
#Inf 3 es la parte inferior del perro desde el talon trasero en adelante
#Para graficar las funciones resultantes es necesario limitar el rango de cada funcion.
xs <- seq(0.5, 17, by = 0.5)
xsup=c(0,0.25,0.5,1,1.5,2,2.35,2.65,2.85,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,8.9,9.1,9.25,9.65,10,10.5,10.85,11.15,
       11.45,11.8,12.15,12.4,12.8,13,13.4,13.6,14,14.3,14.55,14.8,15.1,15.25,15.55,15.8,16,16.3,16.45)
ysup=c(0.45,0.8,1,1,1,1,1,1.1,1.25,1.45,1.95,2.5,2.8,2.95,3,3,2.85,2.6,2.35,2.1,1.8,1.6,1.4,1.6,2.1,2.5,2.85,2.95,2.95,
       2.9,2.8,2.55,2.3,2.05,1.95,2,2.05,2.05,2,1.95,1.8,1.5,1.2,1.2,1.15,1,0.65,0.3)
plot(xsup)

xinf1=c(0,1,2,3,3.5,3.9,4.3)
yinf1=c(0.45,0.4,0.35,0.3,0.3,0.4,0.55)


xinf2=c(3.9,4.1,4.2,4.3)
yinf2=c(0,0.25,0.35,0.55)


xinf3=c(3.9,4.15,4.45,4.75,5.2,5.6,5.9,6.2,6.5,6.75,7,7.15,7.4,7.6,8,8.5,9.25,9.4,9.5,9.8,10,10.25,10.55,11,11.5,
        12,12.6,12.8,13,13.2,13.35,13.8,14.5,14.8,15,15.15,15.45,15.65,15.8,15.95,16,16.25,16.45)
yinf3=c(0,-0.2,-0.3,-0.3,-0.2,-0.15,-0.2,-0.35,-0.4,-0.35,-0.35,-0.3,-0.3,-0.25,-0.25,-0.3,-0.3,-0.4,-0.45,-0.45,
        -0.4,-0.4,-0.4,-0.45,-0.5,-0.6,-0.7,-0.7,-0.65,-0.6,-0.6,-0.6,-0.5,-0.3,-0.15,0.1,0.1,0.1,-0.05,0,0.1,
        0.1,0.3)


f<-cubicspline(xsup, ysup)
sup <- function(xs) ppval(f, xs)


f1<-cubicspline(xinf1, yinf1)
inf1 <- function(xs) ppval(f1, xs)


f2<-cubicspline(xinf2, yinf2)
inf2 <- function(xs) ppval(f2, xs)

f3<-cubicspline(xinf3, yinf3)
inf3 <- function(xs) ppval(f3, xs)


p <- ggplot(data = data.frame(x = c(0,16.45)), mapping = aes(x = x))
p <- p +
  stat_function(fun=sup,aes(colour="Sup")) +
  ylim(-1,4)
p<-p + stat_function(fun=inf1, aes(colour="Inf1"), xlim=c(0,4.3))
p<-p + stat_function(fun=inf2, aes(colour="Inf2"), xlim=c(3.9,4.3))
p<-p + stat_function(fun=inf3, aes(colour="Inf3"), xlim=c(3.9,16.45))
print(p)


