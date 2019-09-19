setwd("C:/Users/mchauveau/Datavisualisation")
library(data.table)
library(stringr)

## Mettre le bon chemin avec les fichiers
path = "C:/Users/mchauveau/Datavisualisation/Region/"
liste_csv = list.files(path)

## Lecture et sauvegarde dans une liste de chaque .csv du dossier
csv = list()
for(i in 1:length(liste_csv)){
  csv[[i]] = fread(paste0(path, liste_csv[i]))
}

## Concaténation par ligne de tout les csv en un
final_2018 = rbindlist(csv)

# suppression des colonnes inutiles (bcp de NA)
col_suppr = c(5,6,8,16,17,18,20,21,22,23,24,25,26,35,37,39,40,46,47)
final_2018[, col_suppr] = NULL

attach(final_2018)


final_2018$adresse = paste(NUMVOIE, INDREP, TYPVOIE, NOMVOIE, CODEPOSTAL, LIBCOM)

reg = final_2018[DEP %in% c(75, 13, 69, 31, 06, 44, 34, 67),]

## Sauvegarde de la table
save(final_2018, file = "./data/final.Rdata")



load(file="./data/final.Rdata")

#final=final_2018


## Association du nom des iles pour les régions DOM-TOM
final_2018[REG %in% c(1,2,3,4,6) ]$LIBREG="DOM-TOM"
final_2018[DEP %in% c(971,972,973,974,976)]$LIBDEP=rep(c("Guadeloupe","Martinique","Guyane","La Reunion","Mayotte"),
                                                       final_2018[LIBREG %in% "DOM-TOM",.N,by="DEP"]$N)

## Ajout des communes pour celles avec une erreur de saisie du type "V"
final_2018[LIBCOM %in% "V" & CODEPOSTAL %in% 16400,]$LIBCOM="Voeuil-et-giget"
final_2018[LIBCOM %in% "V" & CODEPOSTAL %in% 67430,]$LIBCOM="Voellerdingen"
final_2018[LIBCOM %in% "V" & CODEPOSTAL %in% 69200,]$LIBCOM="Venissieux"
final_2018[LIBCOM %in% "V" & CODEPOSTAL %in% 74100,]$LIBCOM="Vetraz-monthoux"
final_2018[LIBCOM %in% "V" & CODEPOSTAL %in% 78140,]$LIBCOM="Velizy-Villacoublay"

## Ajout des communes pour celles avec une erreur de saisie du type ""
final_2018[LIBCOM %in% "" & CODEPOSTAL %in% 28230,]$LIBCOM="Epernon"
final_2018[LIBCOM %in% "" & CODEPOSTAL %in% 51480,]$LIBCOM="Oeuilly"
final_2018[LIBCOM %in% "" & CODEPOSTAL %in% 68124,]$LIBCOM="Wintzenheim"
final_2018[LIBCOM %in% "" & CODEPOSTAL %in% 80860,]$LIBCOM="Noyelles-sur-mer"
final_2018[LIBCOM %in% "" & CODEPOSTAL %in% 91520,]$LIBCOM="Egly"
final_2018[LIBCOM %in% "" & CODEPOSTAL %in% 91000,]$LIBCOM="Evry"

## Ajout des communes pour celles avec une erreur de saisie du type "?"
## AUtomatisation car 41 Communes avec un "?" (difficle de le faire à la mano)
piVille=final_2018[grep("\\?",LIBCOM),]$LIBCOM
unique(piVille)
piVilleOK=c("Boissy-le-Chatel","Carrieres sur Seine","Creteil",'SCHOELCHER',"VENDEUVRES","Chambray-les-Tours",
  "VAUX-LES-PRES","Chalon-sur-Saone","Besancon","CREVECOEUR-LE-GRAND","SAINT-MARTIN-LE-NOEUD",
  "ANNOEULLIN","MARCQ-EN-BAROEUL","MONS-EN-BAROEUL","ROEULX","ESCAUDOEUVRES","NOEUX-LES-MINES",
  "ROEUX","ESCOEUILLES","CREVECOEUR-SUR-L'ESCAUT","JOEUF","VANDOEUVRE-LES-NANCY","HOENHEIM","SCHOENECK",
  "SCHOENAU","WOERTH","GOERSDORF","BOERSCH","VOEGTLINSHOFFEN","Vandoeuvre-les-Nancy","HOEDIC","PLOEUC-L'HERMITAGE",
  "TRESBOEUF","Jurancon","MERCOEUR","COEUR DE CAUSSE","Ales","SAINT-PIERRE-DE-BOEUF","CHAMBOEUF","ecully","Sainte-Foy-les-Lyon","Aubiere",
  "Romans-sur-Isere")
toto=data.frame(table(piVille))
rownames(toto)=toto$piVille
final_2018[grep("\\?",LIBCOM),]$LIBCOM=rep(piVilleOK,toto[unique(piVille),]$Freq)

final_2018$LIBCOM=tolower(final_2018$LIBCOM)


## Simplification des villes avec arrondissement
## exemple : à la place de "lyon 9e arrondissement", on met juste "lyon"
final_2018$LIBCOM[grep("^lyon ",final_2018$LIBCOM)]="lyon"
final_2018$LIBCOM[grep("^paris ",final_2018$LIBCOM)]="paris"
final_2018$LIBCOM[grep("^marseille ",final_2018$LIBCOM)]="marseille"

## Creation d'un tableau par département
departement=unique(setkeyv(final_2018[,c("LIBREG","LIBDEP","LIBCOM","DEP")],c("LIBREG","LIBDEP","LIBCOM","DEP")))

## Nombre de logement sociaux par commune
NB=final_2018[,.N,by=list(LIBCOM,DEP)]

## Merge du nombre de logements sociaux par commune (LIPCOM) avec la région (LIBREG), le code postal (CODEPOSTAL) et le département (LIBDEP) associés
ville=merge(departement,NB,by=c("LIBCOM","DEP"))





## Sauvegarde de la table de données
save(ville,file="./data/ville.Rdata")


densite = fread("./data/villes_france.csv")
densite=densite[,c("V4","V2","V15")]

colnames(densite) = c("Ville","CodePostal","Population")


densite$Ville=tolower(densite$Ville)

save(densite, file = "./data/densite.Rdata")



load(file="./data/densite.Rdata")

densite$Population_municipale_2010=as.numeric(gsub(" ","",densite$Population_municipale_2010))
densite[order(Population_municipale_2010,decreasing = TRUE),]


