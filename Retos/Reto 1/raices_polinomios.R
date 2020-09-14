library(Rmpfr)
options(digits = 22)

#VERSION QUE CALCULA LA PRIMERA Y SEGUNDA DERIVADA EVALUADAS E X
#El vector de coeficientes tiene el de x^i en la posicion i+1
hornerModificado <- function(coeficientes, x){
  res = c(mpfr(0,128),mpfr(0,128),mpfr(0,128))
  grado = length(coeficientes) - 1
  res[1] = coeficientes[grado+1]
  res[2] = coeficientes[grado+1]*grado
  res[3] = res[2]*(grado-1)
  grado = grado - 1
  
  while(grado >= 0){
    if(coeficientes[grado + 1] != 0){
      res[1] = res[1]*x + coeficientes[grado+1]
      if(grado > 0){
        termino = coeficientes[grado+1]*grado
        res[2] = res[2]*x + termino
        if(grado > 1){
          res[3] = res[3]*x + termino*(grado-1)
        }
      }
    }
    else{
      res[1] = res[1]*x
      if(grado > 0){
        res[2] = res[2]*x
        if(grado > 1){
          res[3] = res[3]*x
        }
      }
    }
    grado = grado -1
  }
  return (res)
}

#Método de Horner modificado para calcular también primera y segunda derivada. Utilizado para calcular raíces complejas.
#El vector de coeficientes tiene el de x^i en la posicion i+1
hornerModificadoC <- function(coeficientes, x){
  res = c(0,0,0)
  grado = length(coeficientes) - 1
  res[1] = coeficientes[grado+1]
  res[2] = coeficientes[grado+1]*grado
  res[3] = res[2]*(grado-1)
  grado = grado - 1
  
  while(grado >= 0){
    if(coeficientes[grado + 1] != 0){
      res[1] = res[1]*x + coeficientes[grado+1]
      if(grado > 0){
        termino = coeficientes[grado+1]*grado
        res[2] = res[2]*x + termino
        if(grado > 1){
          res[3] = res[3]*x + termino*(grado-1)
        }
      }
    }
    else{
      res[1] = res[1]*x
      if(grado > 0){
        res[2] = res[2]*x
        if(grado > 1){
          res[3] = res[3]*x
        }
      }
    }
    grado = grado -1
  }
  return (res)
}

#Metodo de Newton que utiliza Horner
#El vector de coeficientes tiene el de x^i en la posicion i+1
newtonHorner <- function(coeficientes,x0,tol,maxiter){
  cat("\n")
  x=mpfr(x0,128)
  i = 1
  error = 1
  anterior = 0
  raiz = c()
  iteraciones = c()
  valor_error = c()
  while(error>tol &&i < maxiter){
    x0=x
    eva = hornerModificado(coeficientes,x0)
    x = x0- eva[1]/eva[2]
    error = abs(x-anterior)
    valor_error <- append(valor_error, as.numeric(error))
    iteraciones <- append(iteraciones,i)
    raiz <- append(raiz,as.numeric(x))
    cat("-Iteración",i,":","\n")
    cat("   * Newton es",formatMpfr(x),"\n")
    cat("\tError: ",formatMpfr(error),"\n\n")
    i = i+1
    anterior=x
  }
  cat("\n")
  plot(iteraciones,valor_error,"l")
  plot(iteraciones,raiz,"l")
  return(x)
}


#Metodo de Newton que utiliza Horner. Calcula raíces complejas.
#El vector de coeficientes tiene el de x^i en la posicion i+1
newtonHornerC <- function(coeficientes,x0,tol,maxiter){
  cat("\n")
  x=as.complex(x0)
  i = 1
  error = 1
  anterior = 0
  while(abs(error) > tol &&i < maxiter){
    x0=x
    eva = hornerModificadoC(coeficientes,x0)
    x = x0- eva[1]/eva[2]
    error = x-anterior
    cat("-Iteración",i,":","\n")
    cat("   * Newton es",x,"\n")
    cat("\tError: ",error,"\n\n")
    i = i+1
    anterior=x
  }
  cat("\n")
  return(x)
}


#Metodo de Laguerre
#El vector de coeficientes tiene el de x^i en la posicion i+1
laguerre <- function(coeficientes,x0,tol,maxiter){
  cat("\n")
  grado = length(coeficientes) - 1
  x=mpfr(x0,128)
  i = 1
  error = 1
  anterior = 0
  a = 1
  raiz = c()
  iteraciones = c()
  valor_error = c()
  while(abs(a)>tol &&i < maxiter){
    x0=x
    eva = hornerModificado(coeficientes,x0)
    g = eva[2]/eva[1]
    h = g^2 - eva[3]/eva[1]
    
    denominadorA = g + sqrt((grado-1)*(grado*h-g^2))
    denominadorB = g - sqrt((grado-1)*(grado*h-g^2))
    
    if(abs(denominadorA) > abs(denominadorB)){
      a = grado/denominadorA
    }
    else{
      a = grado/denominadorB
    }
    
    if(is.nan(a)){
      return(x)
    }
    
    x = anterior - a
    
    valor_error <- append(valor_error, as.numeric(a))
    iteraciones <- append(iteraciones,i)
    raiz <- append(raiz,as.numeric(x))
    
    cat("-Iteración",i,":","\n")
    cat("   * Laguerre es",formatMpfr(x),"\n")
    cat("\tError: ",formatMpfr(a),"\n\n")
    anterior=x
    i = i+1
  }
  cat("\n")
  plot(iteraciones,valor_error,"l")
  plot(iteraciones,raiz,"l")
  return(x)
}

#Metodo de Laguerre. Funciona con raíces complejas.
#El vector de coeficientes tiene el de x^i en la posicion i+1
laguerreC <- function(coeficientes,x0,tol,maxiter){
  cat("\n")
  grado = length(coeficientes) - 1
  x=as.complex(x0)
  i = 1
  error = 1
  anterior = 0
  a = 1
  raiz = c()
  iteraciones = c()
  valor_error = c()
  while(abs(a)>tol &&i < maxiter){
    x0=x
    eva = hornerModificadoC(coeficientes,x0)
    g = eva[2]/eva[1]
    h = g^2 - eva[3]/eva[1]
    
    denominadorA = g + sqrt((grado-1)*(grado*h-g^2))
    denominadorB = g - sqrt((grado-1)*(grado*h-g^2))
    
    if(abs(denominadorA) > abs(denominadorB)){
      a = grado/denominadorA
    }
    else{
      a = grado/denominadorB
    }
    
    x = anterior - a
    
    cat("-Iteración",i,":","\n")
    cat("   * Laguerre es",x,"\n")
    cat("\tError: ",a,"\n\n")
    anterior=x
    i = i+1
  }
  cat("\n")
  return(x)
}



#print(newtonHorner(c(mpfr(-250,128),mpfr(155,128),mpfr(-9,128),mpfr(-5,128),mpfr(1,128)), 0, 1e-32, 50))
#print(laguerre(c(mpfr(-250,128),mpfr(155,128),mpfr(-9,128),mpfr(-5,128),mpfr(1,128)), 0, 1e-32, 50))
#print(newtonHornerC(c(1,2,3,4,5), 5+1i, 1e-32, 1000))
print(laguerreC(c(1,2,3,4,5), 5+1i, 1e-32, 50))

