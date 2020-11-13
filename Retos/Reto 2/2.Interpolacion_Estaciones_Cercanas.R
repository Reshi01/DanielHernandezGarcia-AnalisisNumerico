library(raster)
rQuixX<-c(1,2,3,4,5,6,7,13,14,15,16,17,18,19,36,37,38,39,40,41,42,43,44,57,58,
          59,60,61,62,63,64,65,66,67,68,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,
          128,129,130,131,132,133,134,135,136,137,158,159,160,182,183,206,207,208,209,229,230,231,232,233,234,235,
          236,237,238,239,240,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,
          274,275,276,277,278,279,280,281,282,283,284,285,286,287,297,298,299,300,301,302,303,304,305,306,307,308,
          309,310,311,312,313,314,315,316,317,318,321,322,323,324,325,326,327,328,349,350,351,352,353,366,367,368,
          369,370,371,376,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,
          412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,
          438,439,440,441,442,443,444,445,446,447,448,449,450,451,541,542,543,544,545,590,591,592,593,594,595,596,
          597,598,599,600,601,602,603,604,605,606,607,608,613,614,615,616,638,639,640,641,642,643,661,662,663,664,
          665,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,
          693,694,695,696,697,698,699,700,701,702,703,704,705,708,709,710,711,712,713,714,715,716,717,718,719,720)

rQuixY<-c(26.11,25.59,25.26,25.13,25.13,24.74,
          27.98,42.66,44.67,45.04,44.72,41.88,34.46,31.22,38.56,38.55,39.74,40.87,43.14,40.4,33.98,30.64,28.75,33.26,34.46,
          36.01,36.94,36.71,38.63,38.19,38.72,38.35,33.06,30.11,28.16,41.79,40.1,41.06,37.4,32.63,30.39,28.78,27.41,26.44,
          25.78,25.01,24.4,24,23.39,23.68,23.47,23.72,25.3,27.83,32.05,34.29,36.6,38.96,40.6,41.85,44.64,45.07,39.75,40.81,
          41.23,44.18,43.22,42.83,39.29,40.58,37.44,33.27,41.54,42.68,44.39,44.46,38.12,32.46,30.56,29.36,28.15,27.32,26.9,
          26.3,40.59,41.57,44.54,45,40.6,34.02,31.7,29.92,28.73,27.75,27.31,26.69,25.98,25.83,25.61,25.7,25.43,25.52,
          27.07,31.8,34.27,36.82,37.69,39.2,41.06,43.86,41.04,43.59,40.11,33.97,31.26,29.66,28.26,27.16,26.41,34.85,36.96,
          39.45,39.3,40.35,43.74,42.24,43.06,37.5,33.93,31.32,29.43,28.12,27.03,26.34,25.73,25.27,24.86,24.23,23.64,23.27,
          23,33.78,35.32,37.62,39.7,43.66,45.76,46.76,45.41,40.62,43.61,44.26,41.97,38.46,25.17,26.01,26.6,28.47,32.04,
          33.75,41.44,22.78,27.07,32.32,33.4,36,30.42,30.73,40.85,41.79,42.43,37.5,33.69,30.39,28.37,27.25,26.47,25.68,25.32,
          25.11,25.18,24.92,24.79,24.23,23.68,23.66,25.04,27.73,32.1,31.95,35.82,37.99,27.95,28.11,29.07,27.67,27.29,25.48,
          24.08,23.35,23.3,23.38,23.51,23.34,23.43,23.17,23.4,23.32,23.01,22.95,26.98,28.62,31.35,35.11,33.37,36.57,39.74,
          43.17,44.59,41.78,35.97,32.08,29.74,35.21,34.69,32.96,33.21,31.32,39.88,35.72,35.5,33.06,28.34,25.98,25.16,24.81,
          24.52,24.24,24,23.62,24.02,24.56,24.65,24.62,24.38,26.29,30.58,38.01,40.51,42.71,37.25,39.58,39.65,37.62,32.85,
          29.5,27.53,40.44,41.22,37.36,33.43,30.71,25.92,25.66,25.4,24.79,24.32,24.16,23.93,24.08,24.38,24.11,23.77,24.34,
          28.45,30.69,31.93,31.76,34.39,34.66,37.71,39.43,38.14,32.22,29.15,27.48,26.71,25.06,24.43,24.1,23.67,23.36,23.3,
          23.26,23.11,23.16,23.12,23.77,24.83,26.33,32.44,35,35.5,35.52,33.62,30.74,27.89,26.15,24.67,24.3,24.2,23.96,23.73)


#x es el punto cuyo peso se quiere obtener, xi es el punto que se quiere
#interpolar, p es el parámetro de potencia
peso = function(x, xi, p){
  return(1/pointDistance(x, xi, lonlat = FALSE)^p)
}

#x es el vector de puntos usados para la interpolación, y es el vector de
#valores asociados a esos puntos, xi es el punto que se quiere interpolar,
#p es el parámetro de potencia
idw = function(x, y, xi, p){
  numerador = 0
  denominador = 0
  puntos = length(y)
  for(i in 1:puntos){
    if(x[i,1] == xi[1] && x[i,2] == xi[2]){
      return (y[i])
    }
    else{
      peso = peso(x[i, ], xi, p)
      numerador = numerador + peso*y[i]
      denominador =  denominador + peso
    }
  }
  return(numerador/denominador)
}

#Valores de la temperatura
itatira <- read.csv(file=file.path("Data", "Itatira.csv"))
jati <- read.csv(file=file.path("Data", "Jati.csv"))
quixada <- read.csv(file=file.path("Data", "Quixada.csv"))
quixeramobimTeo <- read.csv(file=file.path("Data","Quixeramobim.csv"))

#Coordenadas de las estaciones
xQuixada = c(-39.03337388,-4.967000076)

xJati = c(-38.99792619,-7.68237997)

xItatira = c(-39.6359847,-4.528938261)

xQuixeramobim = c(-39.28369603,-5.200769967)

tamano = length(itatira[,1])


#plot(rQuixX,rQuixY,"p",col="Red")
#lines(1:720,quixeramobim,col="Blue")

#plot(rQuixX,rQuixY,"p",col="Red")
#lines(1:720,quixeramobim,col="Blue")


errores <- function(v,enc){
  maxAbs=100
  i = 1
  valoresEncontrados = enc
  valoresTeo = v
  media = 0
  errorRel = 0
  total = 0
  errorAbs = 0
  max = 0
  vec = vector(length = length(enc))
  cat(" ","\n")
  while(i <= 720){
    if(v[,1][i] != 0){
      #Calculo error relativo
      errorRel = abs(valoresEncontrados[i]-valoresTeo[,1][i])/valoresTeo[,1][i]
      
      #Calculo error absoluto
      errorAbs = abs(valoresEncontrados[i]-valoresTeo[,1][i])
      
      vec[i] = errorAbs
      
      total = total + errorAbs
      #cat(i,": ",errorAbs)
      #("\n")
      
    }
    
    i = i + 1
  }
  
  
  media = total/length(enc)
  cat("Media de error absoluto es ",media,"\n")
  cat("Error minimo absoluto es ",min(vec),"\n")
  cat("Error maximo absoluto es ",max(vec),"\n")
  cat(" ","\n")
  #return(max(vec))
  return(media)
}

jaccardIndex <- function(A,B){
  cat("\n")
  cat("Indice de jaccard es ",length(intersect(A,B))/length(union(A,B)))
}

pot = seq(0,3,0.01)

#errores(itatira,quexada)
q1 = c()
potsize = length(pot)
jp=1
i=1
m = c()
med = vector(length = length(pot))
while(jp <= potsize){
  n=1
  
  q1 = c()

  for(i in 1:tamano){
    q1 = c(q1, idw(matrix(c(xQuixada, xJati, xItatira),nrow=3,ncol=2,byrow = TRUE), c(itatira[,1][i], jati[,1][i], quixada[,1][i]), xQuixeramobim , pot[jp]))


  }
  med[jp] = errores(quixeramobimTeo,q1)
  

  
  jp = jp+1
}
print(med)
cat("Menor de las medias del error absoluto es ",min(med)," y p= ",pot[which.min(med)],"\n")
plot(pot,med,"l",col="Red", ylab="Media del error absoluto", xlab="p")
lines(c(pot[which.min(med)]), c(min(med)),"p",col="Blue")

tamano = length(itatira[,1])

quixeramobim = c()
for(i in 1:tamano){
  quixeramobim = c(quixeramobim, idw(matrix(c(xQuixada, xJati, xItatira),nrow=3,ncol=2,byrow = TRUE), c(itatira[,1][i], jati[,1][i], quixada[,1][i]), xQuixeramobim, 0.77))
}
plot(rQuixX*100,rQuixY,"p",col="Red", ylab="temperatura", xlab="hora")
lines((1:tamano)*100,quixeramobim,"l",col="Blue")

errores(quixeramobimTeo, quixeramobim)
