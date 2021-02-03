#POSTWORK5
#A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 y 2019/2020, crea el data frame SmallData, que contenga las columnas date, home.team, home.score, away.team y away.score; esto lo puede hacer con ayuda de la función select del paquete dplyr. Luego establece un directorio de trabajo y con ayuda de la función write.csv guarda el data frame como un archivo csv con nombre soccer.csv. Puedes colocar como argumento row.names = FALSE en write.csv.

LE.1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
LE.1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LE.1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = LE.1718, destfile = "LE1718.csv", mode = "wb")
download.file(url = LE.1819, destfile = "LE1819.csv", mode = "wb")
download.file(url = LE.1920, destfile = "LE1920.csv", mode = "wb")

dataFrames <- lapply(dir(), read.csv)
library(dplyr)
newDF <- lapply(dataFrames,select,Date, HomeTeam,FTHG, AwayTeam,FTAG)
newDF <- lapply(newDF, mutate, Date= as.Date(Date,"%d/%m/%y")) #Cambiamos formato de campo Date de char a formato date 
SmallData <- do.call(rbind,newDF)
SmallData <-rename(SmallData, date=Date, home.team=HomeTeam, home.score=FTHG, away.team=AwayTeam, away.score=FTAG)

str(SmallData)
write.csv(SmallData, file="soccer.csv", row.names = FALSE)

#2
library(fbRanks)
listasoccer <- create.fbRanks.dataframes("soccer.csv", date.format = "%Y-%m-%d")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

#3
fecha <- unique(anotaciones$date)
n<- length(fecha)
ranking <- rank.teams(scores=anotaciones, teams = equipos , max.date = max(fecha),min.date = min(fecha), date.format = "%Y-%m-%d")

#4
predict.fbRanks(ranking,date=fecha[n])
