#POSTWORK 4
#Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por el producto de las probabilidades marginales correspondientes.
LE.1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
LE.1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LE.1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = LE.1718, destfile = "LE1718.csv", mode = "wb")
download.file(url = LE.1819, destfile = "LE1819.csv", mode = "wb")
download.file(url = LE.1920, destfile = "LE1920.csv", mode = "wb")

dataFrames <- lapply(dir(), read.csv)
library(dplyr)
newDF <- lapply(dataFrames,select,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)
newDF <- lapply(newDF, mutate, Date= as.Date(Date,"%d/%m/%y")) #Cambiamos formato de campo Date de char a formato date 
finalDF <- do.call(rbind,newDF) #Unimos dataframes de la lista


# Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
numPartidos <- dim(finalDF)[1]
probGolesCasa  <- table(finalDF$FTHG)/numPartidos
probGolesCasa <- round(probGolesCasa,3)
# La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
probGolesVisita <- table(finalDF$FTAG)/numPartidos
probGolesVisita <- round(probGolesVisita,3)
# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
probConjunta <- table(finalDF$FTHG,finalDF$FTAG)/numPartidos
probConjunta <- round(probConjunta,3)

#OBTENCIÓN DE TABLA DE COCIENTES DATOS COMPLETOS
tablaCocientes <- apply(probConjunta, 2, function(columna) columna/probGolesCasa)
tablaCocientes <- apply(tablaCocientes, 1, function(fila) fila/probGolesVisita)
tablaCocientes <- t(tablaCocientes)

#TECNICA DE BOOTSTRAP, TABLAS DE COCIENTES DE MUESTRAS ALEATORIAS DE 300 VALORES
numMatrixes <-1000 #Numero de muestreos
dimTable <-5 #Tamaño de cada tabla
sizeMatrix <-dimTable*dimTable*numMatrixes
matrix <- array (1:sizeMatrix, c(dimTable,dimTable,numMatrixes))

for (i in 1:numMatrixes){
  index <- sample(dim(finalDF)[1], size = 300, replace = TRUE)
  sampleData <- finalDF[index, ]
  probCasa <- round(table(sampleData$FTHG)/dim(sampleData)[1], 3) 
  probVisita <- round(table(sampleData$FTAG)/dim(sampleData)[1], 3) 
  probConjunta<- round(table(sampleData$FTHG, sampleData$FTAG)/dim(sampleData)[1], 3)
  #tablaCocientes
  tablaCocientesSample <- probConjunta/outer(probCasa, probVisita, "*")
  #ALMACENAR DATOS DE TABLA COCIENTES EN MATRIZ
  for (j in 1:dimTable){
    for(k in 1:dimTable){
      matrix[j,k,i]=tablaCocientesSample[j,k]
    }
  }
}

##Graficas de distribucion de cocientes
par(mfrow=c(3,3)) #Mostramos 4 tablas por cada imagen
for (j in 1:dimTable){
  for(k in 1:dimTable){
    vector <- c(1:numMatrixes)
    for (i in 1:numMatrixes){
      vector[i]=matrix[j,k,i] #Asignamos valores de una celda determinada del numero de Matrices
    }
    titulo <- paste(numMatrixes,"muestras","FTHG:",j,"vs ","FTAG:",k)
    hist(vector, xlab="Cocientes",ylab = "Frecuencia",main=titulo)
  }
}
dev.off()
