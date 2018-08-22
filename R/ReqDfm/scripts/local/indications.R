rm(list=ls())

unaccent <- function(text) {
  text <- gsub("['`^~\": ]", "_", text)
  text <- gsub("[éèëê]", "e", text)
  text <- gsub("[àäâ]", "a", text)
  return(text)
}
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

getConcatenation = function(df){
  df <- unique(df)
  concatenation <- paste(df[,1],collapse=" ")
  return(concatenation)
}
create_df_dfm<- function (csv) {
  df <- read.csv(file = csv, stringsAsFactors = F, encoding = "UTF-8", header = F)
  vec<-getConcatenation(df)
  stopwords.fr<-quanteda::stopwords(language = "fr", source = "stopwords-iso")
  dfm <- quanteda::dfm(vec, remove = stopwords.fr, remove_punct=TRUE, remove_numbers = TRUE, tolower = TRUE, verbose = TRUE)
  return(c(df, dfm))
}
getIndication = function(BN){
  bool <- grepl(BN, indications$medicament, ignore.case = T)
  sum(bool)
  if (!any(bool)){
    cat(BN, " non trouvé")
    return(data.frame(indicationCode=character(), indication=character(), indicationLong=character()))
  }
  BNindications <- subset (indications, bool,select=c("indication (court)","indication (code)","indication (lib)"))
  BNindications <- unique(BNindications)
  return(BNindications)
}

# assign(BN,BNindications)



indications <- read.table("indications/theriaque_indication_cim10.csv",sep=",", header=F,quote = "\"",encoding = "ISO-8859-1", stringsAsFactors = F)

head(indications)
colnames(indications)
colnames(indications) <- c("medicament","indication (court)","indication (code)","indication (lib)")

listeMedicamentTest <- c("methotrexate","baclofene","ibuprofene","voltarene","spasfon", "quinimax")

assign(paste0("indication",listeMedicamentTest[1]),getIndication(listeMedicamentTest[1]))


indicMetho<-unique(indicationMetho$'indication (code)')

indicationBaclo <- getIndication(listeMedicamentTest[2])
indicBaclo<-unique(indicationBaclo$'indication (code)')

indicationIbupr <- getIndication(listeMedicamentTest[3])
indicIbupr<-unique(indicationIbupr$'indication (code)')

indicationVoltar <- getIndication(listeMedicamentTest[4])
indicVoltar<-unique(indicationVoltar$'indication (code)')

indicationSpasf <- getIndication(listeMedicamentTest[5])
indicSpasf<-unique(indicationSpasf$'indication (code)')

indicationQuinim <- getIndication(listeMedicamentTest[6])
indicQuinim <- unique(indicationQuinim$'indication (code)')