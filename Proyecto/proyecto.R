library("deSolve")
#library("tseriesChaos")
#library("nonlinearTseries")

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
c = 5
f = 30

pars  <- c(xgoal = xg,
          ygoal = yg,
          xhazard = xh,
          yhazard = yh,
          c = c,
          f = f)
ini  <- c(x = xi, y = yi)
times <- seq(0, 20, by = 1)
out   <- ode(ini, times, pathFinder, pars, method = "adams")
#out   <- ode(ini, times, pathFinder, pars, method = "adams")
summary(out)

## Default plot method
plot(out[,"x"], out[, "y"], "l", xlab="Posicion X", ylab="Posicion Y")
points(xg, yg,col="green")
points(xi, yi,col="blue")
points(xh, yh,col="red")

xs=c(xg,xi,xh)
ys=c(yg,yi,yh)
lab=c("Meta","Inicio","Obstaculo")
text(xs, ys, labels=lab, cex= 0.7, pos=3)

#Exponente de Lyapunov
#output <-lyap_k(out[,"x"], m=2, d=2, ref=10, t=40, s=200, eps=4)
#lyap(output, 0.73, 2.47)
#plot(output)