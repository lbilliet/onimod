library(stringr)
library(quanteda)

endpoint <- "http://172.16.90.22:8089/bigdata/namespace/doctissimoFullIndex/sparql"

source("fonctionDatesSearchMedicament.R")

getConcatenation = function(term){
  df <- search_medicament(endpoint, term)
  df <- unique(df)
  concatenation <- paste(df$contentPost,collapse="\n")
  #matrice <- quanteda::dfm(concatenation, remove = stopwords.fr, remove_punct=TRUE, remove_numbers = TRUE, 
   #             tolower = TRUE, verbose = TRUE)
  return(concatenation)
}

## recherche d'un médicament : 
listeMedicamentTest <- c("methotrexate","baclofene","ibuprofene","voltarene","spasfon")
nomMedicament <- "spasfon"

df <- search_medicament(endpoint, nomMedicament)
df <- unique(df)

### charger les indications
source("indications.R")

indicationsSelection <- lapply(listeMedicamentTest, function(x){
  getIndication(x)
})

httr::GET(url = "http://91.121.106.228:8891/CTapi-0.0.1-SNAPSHOT/CTstringInputBody")


stopwords.fr<- quanteda::stopwords(language = "fr", source = "stopwords-iso")
colnames(df)
geu <- paste0(df$contentPost)

geuContent <-getConcatenation("geu")
methotrexateContent <- getConcatenation("methotrexate")
psoriasisContent <- getConcatenation("psoriaris")
length(geuContent)
unCorpus <- corpus(c(geuContent, methotrexateContent, 
                     psoriasisContent))
matrice <- quanteda::dfm(unCorpus, 
                         remove = stopwords.fr, remove_punct=TRUE, remove_numbers = TRUE, 
                                      tolower = TRUE, verbose = TRUE)
dist<-textstat_simil(matrice, margin="documents", method="cosine")
dist
as.data.frame(as.matrix(dist))



contreIndication = function(medicament, maladie){
  dfMedicament <- search_medicament(endpoint, medicament)
  dfMedicamentId <- unique(dfMedicament$postURL)
  
  bool <- grepl(maladie, dfMedicament$contentPost)
  if (!any(bool)){
    cat("aucun post en commun !")
    return(NULL)
  }
  dfMaladie <- subset (dfMedicament, bool)
  return(dfMaladie)
  # dfMaladie <- search_medicament(endpoint, maladie)
  # dfMaladieId <- unique(dfMaladie$postURL)
  # 
  # communsId <- intersect(dfMaladieId, dfMedicamentId)
  # if (length(communsId) == 0){
  #   cat("aucun post en commun !")
  #   return(NULL)
  # }
  # postsCommuns <- subset (dfMedicament, postURL %in% communsId)
  # return(postsCommuns)
}


voir <- contreIndication("depakine","grossesse")
