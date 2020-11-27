#Santiago Caroprese, Daniel Hernandez y Juan Carlos Suarez
library("deSolve")
library("Rmpfr")
library(pracma)
library(raster)

pathFinder <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    #H = (x-xgoal)^2 + (y-ygoal)^2 + 10/(((1/c)*(x-xhazard))^2 + ((1/c)*(y-yhazard))^2 + 1/f)
    dx = -2*(-xgoal + x) + (20*(-xhazard + x))/(c^2*(f^(-1) + (-xhazard + x)^2/c^2 + (-yhazard + y)^2/c^2)^2)
    dy = -2*(-ygoal + y) + (20*(-yhazard + y))/(c^2*(f^(-1) + (-xhazard + x)^2/c^2 + (-yhazard + y)^2/c^2)^2)
    return(list(c(dx, dy)))
  })
}

xi = 10
yi = 10
xg = 2
yg = 1
xh = 5
yh = 7
c = 10
f = 30

pars  <- c(xgoal = xg,
           ygoal = yg,
           xhazard = xh,
           yhazard = yh,
           c = c,
           f = f)
ini  <- c(x = xi, y = yi)
times <- seq(0, 20, by = 0.1)
out   <- ode(ini, times, pathFinder, pars, method = "euler")
outA   <- ode(ini, times, pathFinder, pars, method = "adams")

plot(out[,"x"], out[, "y"], "l", xlab="X", ylab="Y")

xs=c(xg,xi,xh)
ys=c(yg,yi,yh)
lab=c("Meta","Inicio","Obstaculo")
text(xs, ys, labels=lab, cex= 0.7, pos=3)

points(xg, yg,col="green")
points(xi, yi,col="blue")
points(xh, yh,col="red")


distancia = 0
i = 1
tam = length(out[, "time"])
print(out)
while(i < tam){
  set1 <- cbind(c(out[,"x"][i]),c(out[,"y"][i]))
  set2 <- cbind(c(out[,"x"][i+1]),c(out[,"y"][i+1]))
  #print(set1)
  #print(set2)
  distancia = distancia + pointDistance(set1,set2,lonlat=FALSE)
    i = i+1

}
cat("trayectoria euler ",distancia,"\n")
set1 <- cbind(c(2),c(1))
set2 <- cbind(c(out[,"x"][tam]),c(out[,"y"][tam]))
#cat("Inicio ",set1,"\n")
#cat("Final ",set2,"\n")
cat("total ",formatMpfr(mpfr(pointDistance(set1,set2,lonlat=FALSE),128)),"\n")
distancia = 0
i=1
tam = length(outA[, "time"])
while(i < tam){
  set1 <- cbind(c(outA[,"x"][i]),c(outA[,"y"][i]))
  set2 <- cbind(c(outA[,"x"][i+1]),c(outA[,"y"][i+1]))
  #print(set1)
  #print(set2)
  distancia = distancia + pointDistance(set1,set2,lonlat=FALSE)
  i = i+1
#
}
cat("trayectoria adams ",distancia,"\n")
set1 <- cbind(c(2),c(1))
set2 <- cbind(c(outA[,"x"][tam]),c(outA[,"y"][tam]))
#cat("Inicio ",set1,"\n")
#cat("Final ",set2,"\n")
cat("total ",formatMpfr(mpfr(pointDistance(set1,set2,lonlat=FALSE),128)),"\n")