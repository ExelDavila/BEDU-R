#POSTWORK7
#Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como match
#Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la base
install.packages('mongolite')
library(mongolite)
library(jsonlite)

dataBase <- mongo(
  collection = "match",
  db = "match_games",
  url = "mongodb+srv://ExelDavila:XAT5aj1anODXvgiR@beducluster.lnis6.mongodb.net/",
  verbose = FALSE,
  options = ssl_options()
)

#Conteo de archivos
dataBase$count(query = '{}')

#Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el número de goles que metió el Real Madrid el 20 de diciembre de 2015 y contra que equipo jugó, ¿perdió ó fue goleada?
result <- dataBase$find(query = '{"date":"2015-12-20","$or":[{"home.team":"Real Madrid"},{"away.team":"Real Madrid"}]}',fields = '{"_id" : 0,"home.team":1,"home.score":1, "away.team":1,"away.score":1}') 
result
#Se observa que el Real Madrid ganó por goleada 10 a 2 contra Vallecano

#Por último, no olvides cerrar la conexión con la BDD
dataBase$disconnect(gc = TRUE)
