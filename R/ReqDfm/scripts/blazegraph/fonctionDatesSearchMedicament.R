#' Cette fonction permet de transformer une date SIOC au format R
#' @param variableDate : valeur string d'une date au format RDF : Année-Mois-Jour
#'
stringSIOCtoDate <- function(variableDate){
  require(stringr)
  toDate <- str_extract(variableDate,"[0-9]{4}-[0-9]{2}-[0-9]{2}")
  toDate <- as.Date(toDate, format="%Y-%m-%d")
  return(toDate)
}

#' Cette fonction permet de recuperer des posts contenant le nom des medicaments recherches sur le triplestore de Bruno
#' @param endpoint : triplestore SPARQL
#' @param termeRecherche : recherche fulltext dans ce triplestore
#' 
search_medicament <- function(endpoint, termeRecherche){
  ## OR
  termeRecherche <- paste (termeRecherche, collapse=" | ")
  ## en dur ! ne pas modifier cette requete
  requete <- readLines("requete/requeteMedicament")
  requete <- gsub("MEDICAMENT",termeRecherche,requete)
  writeLines(requete, "tempRequete")
  fichierSortie <- "tempOutput.csv"
  commande <- paste ("curl -g --data-urlencode query@tempRequete ",
                     endpoint,
                     " -H 'Accept: text/tab-separated-values' > ", fichierSortie,sep="")
  cat ("lancement de la commande pour récupérer les posts du terme recherché : ", termeRecherche,"\n")
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





