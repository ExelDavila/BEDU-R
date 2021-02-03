#POSTWORK-SESION2
# Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
LE.1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
LE.1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LE.1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = LE.1718, destfile = "LE1718.csv", mode = "wb")
download.file(url = LE.1819, destfile = "LE1819.csv", mode = "wb")
download.file(url = LE.1920, destfile = "LE1920.csv", mode = "wb")

dataFrames <- lapply(dir(), read.csv)

# Obten una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary
str(dataFrames)
head(dataFrames)
View(dataFrames[[1]]); View(dataFrames[[2]]); View(dataFrames[[3]]);
summary(dataFrames[[1]]); summary(dataFrames[[2]]); summary(dataFrames[[3]]);
# Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).
library(dplyr)
newDF <- lapply(dataFrames,select,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)
# Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).
newDF <- lapply(newDF, mutate, Date= as.Date(Date,"%d/%m/%y")) #Cambiamos formato de campo Date de char a formato date 
finalDF <- do.call(rbind,newDF) #Unimos dataframes de la lista
str(finalDF)
