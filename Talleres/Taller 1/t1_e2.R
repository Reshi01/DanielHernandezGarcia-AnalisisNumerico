library(pracma)
library(rootSolve)
f1 <- function(x)(6+2.13*x^2-0.0013*x^4)
e <- (expression)(6+2.13*x^2-0.0013*x^4)
e2 <- (expression) (x^3 - 2*x^2 + (4*x/3)-(8/27))
devF <-function(x)(2.13 * (2 * x) - 0.0013 * (4 * x^3))


#Punto 1
matrixF <-function(n,sup){
  
  M <- matrix.default(c(1:n^2), nrow = n, byrow = TRUE)
  print(M)
  i = 1
  anterior = 0
  pos = 2
  teo = (n^2/2 + n/2)
  val = 0
  veces = 0

  while (i <= n){
    
    if(sup == TRUE){
      pos = i+1
      while (pos <= n && i < pos) {
        anterior = val
        val = sum(M[i,pos],val)
        pos = pos+1
        veces = veces +1
       
      }
      
    }else{
      pos = 1
      while(pos < i){
        anterior = val
        val = sum(M[i,pos],val)
        pos = pos+1
        veces = veces +1
        
      }
      
    }
    
    i = i+1
  }
  cat("Veces ",veces,"\n")
  cat("Suma da ",val)

}

#Punto 2
sumarN2 <-function(n){
  
  m = n^2
  i = 1
  added = 0
  while(i <= m){
    added = added + i^2
    i=i+1
  }
  cat("Resultado es ", added)
}

#Punto 3
maxAltura <-function(f){
  fx=expression(6+2.13*x^2-0.0013*x^4)
  g <- D(fx,'x')
  print(g)
  val = uniroot.all(devF ,c(-40,50), tol = 1e-32,maxiter = 40)
  max=-100
  for(root in val){
    if(f1(root) > f1(max) && root >= 0){
      max = root
    }
    
  }

  cat("En t = ",max," el cohete alcanza su maxima altura de ",f(max))
}
maxAltura(e)



#n^2 /2 - n/2

 