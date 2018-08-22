rm(list=ls())

# library(stringr)
library(quanteda)
library(plyr)

wd<-function (){
  rm(list = ls())
  chemRepTr <- getwd()
  resRchYafExt <- grep("local", chemRepTr, ignore.case = T)
  resRchYafInt <- grep("lb3", chemRepTr, ignore.case = T)
  resRchTourLin <- grep("/home/louis", chemRepTr, ignore.case = T)
  resRchTourWin <- grep("F:/", chemRepTr, ignore.case = T)

  if (any(resRchYafExt)) {
    setwd("C:/Users/lb3_local/OneDrive/sync/git/")
    chemRepTr
  } else if (any(resRchYafInt)) {
    setwd("C:/Users/lb3/OneDrive/sync/git/")
    chemRepTr
  } else if (any(resRchTourWin)) {
    setwd("F:/OneDrive/sync/git/")
    chemRepTr
  } else if (any(resRchTourLin)) {
    setwd("~/git/")
    chemRepTr
  }
  rm(list=ls())
  return(getwd())
}
wd()
setwd("loubill/Domino/R/ReqDfm")

### charger les indications
source("scripts/local/indications.R")

### créer dfm depuis fichiers csv générés via blazegraph
listTemp<-create_df_dfm("data/mtx.csv") #ici futur df issu requête recherche posts du médicament => modifier la source dans la fonction
char_metho <- listTemp[[1]]
df_metho <- as.data.frame(listTemp[[1]])
dfm_metho <- listTemp[[2]]

length(listTemp)
names(listTemp) <- c("V1","autre")
listTemp[["autre"]]

listTemp <- create_df_dfm("data/alt.csv")#ici futur df issu requête posts des indications du médicament => modifier la source dans la fonction + faire idem pour post CI
char_allait <- listTemp[[1]]
df_allait <- as.data.frame(listTemp[[1]])
dfm_allait <- listTemp [[2]]

indicationsSelection <- lapply(listeMedicamentTest, function(x){
  getIndication(x)
})

# httr::GET(url = "http://91.121.106.228:8891/CTapi-0.0.1-SNAPSHOT/CTstringInputBody")

stopwords.fr<- quanteda::stopwords(language = "fr", source = "stopwords-iso")
head(colnames(dfm_metho))
head(df_metho)
head(colnames(dfm_allait))
head(df_allait)

# geu <- paste0(df$contentPost)
# geuContent <-getConcatenation("geu")
# methotrexateContent <- getConcatenation("methotrexate")
# psoriasisContent <- getConcatenation("psoriaris")

unCorpus <- corpus(c(char_metho, char_allait))
matrice <- quanteda::dfm(unCorpus, 
                         remove = stopwords.fr, remove_punct=TRUE, remove_numbers = TRUE, 
                                      tolower = TRUE, verbose = TRUE)
dist<-textstat_simil(matrice, margin="features", method="cosine")
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
