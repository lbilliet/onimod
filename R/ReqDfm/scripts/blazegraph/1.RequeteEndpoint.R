rm(list=ls())
library(stringr)
## adresse de l'endpoint SPARQL
# endpoint <- "http://172.16.90.22:8889/bigdata/namespace/doctissimoGrossesse_2010-2015_fullindex/sparql"
endpoint <- "http://172.16.90.22:8089/bigdata/namespace/doctissimoFullIndex/sparql"

source("fonctionDates.R")

## recherche d'un médicament : 
nomMedicament <- c("KLIPAL CODEINE")
nomMedicament <- c("METHOTREXATE")

# télécharger les posts au format CSV, puis les charger ici : 
undebug(search_medicament)
df <- search_medicament(endpoint, nomMedicament)
df <- unique(df)

function(endpoint, nomMedicament){
  ## OR
  nomMedicament <- paste (nomMedicament, collapse=" | ")
  ## en dur ! ne pas modifier cette requete
  requete <- readLines("requete/requeteMedicament")
  requete <- gsub("MEDICAMENT",nomMedicament,requete)
  writeLines(requete, "tempRequete")
  fichierSortie <- "tempOutput.csv"
  commande <- paste ("curl -g --data-urlencode query@tempRequete ",
                     endpoint,
                     " -H 'Accept: text/tab-separated-values' > ", fichierSortie,sep="")
  cat ("lancement de la commande pour récupérer les posts du médicament : ", nomMedicament,"\n")
  system(commande)
  #### fichier temporaire de sortie
  df <- read.table("tempOutput.csv",sep="\t",header=T,quote="",comment.char = "")
   colnames(df) <- gsub("^X.","",colnames(df))
  df$datePost <- stringSIOCtoDate(df$datePost)
  df$age <- stringSIOCtoDate(df$age)
  df$age <- round(difftime(df$datePost,df$age,units="weeks")/52.25,0)
  df$cosine <- as.numeric(str_extract(df$cosine,"[0-9].[0-9]+"))
  df <- df[order(-df$cosine),]
  
  ### Extraire les catégories (ne fonctionne que pour Doctissimo) :
  cat ("Extraction des catégories Doctissimo")
  df$threadURL2 <- gsub("<http://forum.doctissimo.fr/","",df$threadURL)
  df$categorie  <- str_extract(df$threadURL2 ,"[^/]+")
  df$categorie2  <- gsub("^[^/]+/","",df$threadURL2)
  df$categorie2  <- str_extract(df$categorie2 ,"[^/]+")
  return(df)
}


## charger les référentiels pour avoir la liste des médicaments :
# il y en a deux 
referentiel <- read.table("drugbase/data-sources/referentiel.csv",sep="\t",header=T,quote="")
referentiel2 <- read.table("drugbase/data-sources/medic_nl_20151124.csv",sep="\t",quote="",header=T)
colnames(referentiel2) <- c("codeATC","libATC","nomCourt")

#### il y a 2 référentiels où on a le nom des produits
## dans le premier referentiel, le nom du produit n'est pas normalisé
## on veut utiliser le premier référentiel car on a les codes CIP (on peut trouver les codes CIS)
## on veut utiliser le 2ème référentiel car on a le nom normalisé
bool <- referentiel$code_atc %in% referentiel2$codeATC
sum(bool) ### 19 986 codes ATC en commun
ref <- merge (referentiel, referentiel2, by.x="code_atc",by.y="codeATC")
ref <- subset (ref, select=c(cip13,CIP7,nomCourt, code_atc,libATC))
ref <- unique(ref)
length(unique(ref$nomCourt)) #### 2 446 noms de produits uniques


############## Ajout du code CIS pour faire le lien vers les RCP : #####################################
CIS <- read.table ("CIS_CIP_bdpm.txt",sep="\t",encoding="ISO8559-1",quote="")
CIS <- subset (CIS,select=c(V1,V2,V7))
colnames(CIS) <- c("CIS","CIP7","CIP13")
bool <- ref$CIP7 %in% CIS$CIP7
sum(bool)
sum(!bool) ## certains n'ont pas de code CIS
ref <- merge (ref, CIS, by="CIP7")

produitunique <- unique(ref$nomCourt) ##
length(unique(produitunique)) #### 2 346produits uniques
ref$cip13 <- NULL
colnames(ref) <- c("CIP7","nomCourt","codeATC","libATC","CIS","CIP13")
save(ref, file="ref.rdata")


# ce fichier provient de : home/ERIAS/Documents/sebastien/Rdrugsafe/contreindication/enceinte.R :
CISgrossesseCI <- readLines("CISgrossesseCI.txt")
bool <- ref$CIS %in% CISgrossesseCI
produitsCIgrossesse <- sort(unique(ref$nomCourt[bool]))
produitsCIgrossesse <- as.character(produitsCIgrossesse)
caracteres <- nchar(produitsCIgrossesse)
table(caracteres)
bool <- caracteres > 4
produitsCIgrossesse <- produitsCIgrossesse[bool]
alldfs <- NULL

#### récupérer des posts avec des médicaments CI ou déconseillés pdt la grossesse : 
system.time(
  for (i in produitsCIgrossesse){
    df <- search_medicament(endpoint, i)
    cat(nrow(df), i, "\n")
    if (nrow(df) != 0){
      df$produit <- i
      alldfs <- rbind (alldfs, df)
    }
  }
)
### environ 6 minutes pour récupérer 12 732 posts (181 médicaments)

# save(alldfs,file="GrossesseCImedicaments.rdata")
# on passe maintenant à l'analyse