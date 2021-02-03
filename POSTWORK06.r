#POSTWORK 06
install.packages('broom')
install.packages("tidyr")
install.packages("doBy")
library(dplyr)
library(tidyverse)
library(doBy)
library(tidyr)
# Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
data <- read.csv("match.data.csv")
# Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
data$sumagoles <- rowSums (data[ ,c("home.score","away.score")])
head(data)
# Obtén el promedio por mes de la suma de goles.
data <- mutate(data, date = as.Date(date,"%Y-%m-%d"))
sdata2 <- group_by(data,Year = format(data$date,'%Y'),Month= format(data$date,'%m'))
agrupados <- as.data.frame(summaryBy(sumagoles ~ Year + Month, data=sdata2, FUN=c(mean)))
agrupados$Day <- rep("01", dim(agrupados)[1])
# Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
agrupados <- as.data.frame(unite(agrupados, Fecha, c(1:2,4),sep="-",remove=TRUE))
agrupados <- mutate(agrupados, Fecha = as.Date(Fecha,"%Y-%m-%d"))
serieTiempo <- ts(agrupados$sumagoles.mean,start = c(2010,08),frequency = 10, end = c(2020,07))
serieTiempo <- ts(agrupados$sumagoles.mean,frequency = 10)

# Grafica la serie de tiempo.
ts.plot(serieTiempo,xlab="Fecha",ylab="Promedio de goles",main="Serie de tiempo")
abline(h=mean(serieTiempo),lwd=3,col=2)
