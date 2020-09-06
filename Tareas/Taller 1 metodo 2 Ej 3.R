digitos=20
#n es el grado del polinomio, x es el valor a evaluar y c es un vector con los coeficientes de tamano
evaluarMetodoDos <- function(n,x,c){
  v = rep(1,n+1)
  v[1]=c[1]
  v[2]=x*c[2]
  mult=1
  for(i in 3:(n+1)){
    v[i]=v[i-1]*x*c[i]
    mult =mult+2
  }
  cat("Numero de Multiplicaciones: ",mult,"\n")
  return(signif(sum(v),digitos))
}

exEq= function(n,x){
  return(signif((x^51-1)/(x-1),digitos))
}

c=rep(1,51)

evaluacion=evaluarMetodoDos(50,1.0001,c)
eExEq=exEq(50,1.0001)
erAbs=abs(signif(evaluacion-eExEq,10))
erReal=signif(erAbs/eExEq,10)

cat("Evaluacion metodo 2: ",evaluacion,"\n")
cat("Expresion Equivalente: ",eExEq,"\n")
cat("Error Absoluto: ",erAbs,"\n")
cat("Error Relativo: ",erReal,"\n")