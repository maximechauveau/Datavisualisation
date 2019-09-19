# 
# PREPARATION DONNEES 2016
# 
library(leaflet)
library(dplyr)
library(foreign)
library(rgdal)
library(plotly)

getwd()
setwd("C:/Users/mchauveau/Datavisualisation")
#
# FICHIER 2016
# 


# on charge le fichier brut 2016
# on filtre sur la ville de nantes
df2016 <- read.csv2('./data/rpls2016_detail_pays_de_la_loire.csv')
df2016N <- df2016 %>% filter(libcom_red == 'Nantes')

# on crée des fichiers csv pour traitement coordonnées gps (max 6mo)
write.csv(x = df2016N[1:20000,], file = "./data/monFichier1.csv")
write.csv(x = df2016N[20001:33794,], file = "./data/monFichier2.csv")

#Convertir fichiers sur https://adresse.data.gouv.fr/csv

# on charge les fichiers avec coordonnées gps
dfNantes2016_1 <- read.csv2('./data/monFichier2016_1.geocoded.csv')
dfNantes2016_2 <- read.csv2('./data/monFichier2016_2.geocoded.csv')

# on fusionne les fichiers 
dfNantes2016 <- rbind(dfNantes2016_1, dfNantes2016_2)

# on prepare les colonnes GPS
dfNantes2016$latitude <- as.character(dfNantes2016$latitude)
dfNantes2016$latitude = gsub(",", ".", dfNantes2016$latitude)
dfNantes2016$latitude <- as.numeric(dfNantes2016$latitude)

dfNantes2016$longitude <- as.character(dfNantes2016$longitude)
dfNantes2016$longitude = gsub(",", ".", dfNantes2016$longitude)
dfNantes2016$longitude <- as.numeric(dfNantes2016$longitude)



#
# FICHIER 2017
# 

# on charge le fichier brut 2017
# on filtre sur la ville de nantes
df2017 <- read.csv2('./data/RPLS2017_geoloc_detail_reg52.csv')
df2017N <- df2017 %>% filter(libcom17 == 'NANTES')

# on crée des fichiers csv pour traitement coordonnées gps (max 6mo)
write.csv(x = df2017N[1:15000,], file = "./data/monFichier2017_1.csv")
write.csv(x = df2017N[15001:21904,], file = "./data/monFichier2017_2.csv")

# on charge les fichiers avec coordonnées gps
dfNantes2017_1 <- read.csv2('./data/monFichier2017_1.geocoded.csv')
dfNantes2017_2 <- read.csv2('./data/monFichier2017_2.geocoded.csv')

# on fusionne les fichiers 
dfNantes2017 <- rbind(dfNantes2017_1, dfNantes2017_2)

# on prepare les colonnes GPS
dfNantes2017$latitude <- as.character(dfNantes2017$latitude)
dfNantes2017$latitude = gsub(",", ".", dfNantes2017$latitude)
dfNantes2017$latitude <- as.numeric(dfNantes2017$latitude)

dfNantes2017$longitude <- as.character(dfNantes2017$longitude)
dfNantes2017$longitude = gsub(",", ".", dfNantes2017$longitude)
dfNantes2017$longitude <- as.numeric(dfNantes2017$longitude)


#
# FICHIER 2018
# 


# on charge le fichier brut 2017
# on filtre sur la ville de nantes
df2018 <- read.csv2('./data/RPLS2018_detail_reg52.csv')
df2018N <- df2018 %>% filter(LIBCOM == 'NANTES')

# on crée des fichiers csv pour traitement coordonnées gps (max 6mo)
write.csv(x = df2018N[1:19000,], file = "./data/monFichier2018_1.csv")
write.csv(x = df2018N[19001:35319,], file = "./data/monFichier2018_2.csv")

# on charge les fichiers avec coordonnées gps
dfNantes2018_1 <- read.csv2('./data/monFichier2018_1.geocoded.csv')
dfNantes2018_2 <- read.csv2('./data/monFichier2018_2.geocoded.csv')

# on fusionne les fichiers 
dfNantes2018 <- rbind(dfNantes2018_1, dfNantes2018_2)

# on prepare les colonnes GPS
dfNantes2018$latitude <- as.character(dfNantes2018$latitude)
dfNantes2018$latitude = gsub(",", ".", dfNantes2018$latitude)
dfNantes2018$latitude <- as.numeric(dfNantes2018$latitude)

dfNantes2018$longitude <- as.character(dfNantes2018$longitude)
dfNantes2018$longitude = gsub(",", ".", dfNantes2018$longitude)
dfNantes2018$longitude <- as.numeric(dfNantes2018$longitude)

# sauvegarde des fichiers rdata
save(dfNantes2016, dfNantes2017,dfNantes2018, file = "./data/dataGPS.RData")

# chargement fichier Rdata
load("./data/dataGPS.RData")

# affichage carte

nantesLong <- -1.5533600
nantesLat <- 47.2172500



# dfNantes$latitude <- as.character(dfNantes$latitude)
# dfNantes$longitude <- as.character(dfNantes$longitude) 

points2016 <- unique(data.frame(longitudes = dfNantes2016$longitude,
                     latitudes = dfNantes2016$latitude,année = '2016', DPE = dfNantes2016$DPEENERGIE_red
                     ))

points2017 <- unique(data.frame(longitudes = dfNantes2017$longitude,
                                latitudes = dfNantes2017$latitude, année = '2017', DPE = dfNantes2017$DPEENERGIE_red
))

points2018 <- unique(data.frame(longitudes = dfNantes2018$longitude,
                                latitudes = dfNantes2018$latitude, année = '2018', DPE = dfNantes2018$DPEENERGIE
))







# affichage proportion classe energetique immeuble

points2016$DPE[points2016$DPE == ''] <- NA
points2016$DPEGrad[points2016$DPE %in% c('A','B')] <- 'Bien isolé'
points2016$DPEGrad[points2016$DPE %in% c('C','D')] <- 'Moyennement isolé'
points2016$DPEGrad[points2016$DPE %in% c('E','F','G')] <- 'Mal isolé'
table_ener <- table(points2016$DPEGrad)
p <- plot_ly(points2016, labels = names(table_ener), values = table_ener, type = 'pie', marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)))
p

points2017$DPE[points2017$DPE == ''] <- NA
points2017$DPEGrad[points2017$DPE %in% c('A','B')] <- 'Bien isolé'
points2017$DPEGrad[points2017$DPE %in% c('C','D')] <- 'Moyennement isolé'
points2017$DPEGrad[points2017$DPE %in% c('E','F','G')] <- 'Mal isolé'
table_ener <- table(points2017$DPEGrad)
p <- plot_ly(points2017, labels = names(table_ener), values = table_ener, type = 'pie', marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)))
p

points2018$DPE[points2018$DPE == ''] <- NA
points2018$DPEGrad[points2018$DPE %in% c('A','B')] <- 'Bien isolé'
points2018$DPEGrad[points2018$DPE %in% c('C','D')] <- 'Moyennement isolé'
points2018$DPEGrad[points2018$DPE %in% c('E','F','G')] <- 'Mal isolé'
table_ener <- table(points2018$DPEGrad)
p <- plot_ly(points2018, labels = names(table_ener), values = table_ener, type = 'pie', marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)))
p

points <- rbind(points2016, points2017, points2018)

# evolution nombre HLM période 2016-2018
table_evo <- table(points$année)
p <- plot_ly(y=table_evo, x=names(table_evo), type='bar')
p

# evol2016-2018
evolution <- 100*(nrow(points2018)-nrow(points2016))/nrow(points2016)

#cartographie logements mal isolés

hlmMalIso <- points %>% filter(DPEGrad == 'Mal isolé')

m <- leaflet(hlmMalIso) %>%
  addTiles() %>%
  setView(lng = nantesLong, lat = nantesLat, zoom = 14)  %>%
  addMarkers(lng = ~longitudes, lat = ~latitudes, popup = ~labels)
m

# cartographie emplacement HLM
m <- leaflet(points2017) %>%
  addTiles() %>%
  setView(lng = nantesLong, lat = nantesLat, zoom = 12)  %>%
  addMarkers(lng = ~longitudes, lat = ~latitudes, popup = ~labels)
m







dbf <- read.dbf("car_m.dbf")

car_m <- readOGR ('car_m.mif', layer="car_m")
