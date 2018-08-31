rm(list=ls())
library(dplyr)
library(tidytext)
load("dfDiseaseRivotril.rdata")
load("dfDrugRivotril.rdata")
load("dfATCRivotril.rdata")
dfBoth <- rbind(dfDrug,dfDisease)
dfBoth <- rbind(dfBoth,dfATC)

### create DocumentTermMatrix
dtmBoth <- dfBoth %>%
  cast_dtm(document, ct, TFIDF,weighting = function(x) {return (x)})
## create a dfmObject : 
dfmBoth <- quanteda::as.dfm(dtmBoth)
drugURI <- "BNj58jf8aqth9bis3rfloeqf278re9l3u7"

## Motlium 
drugURI <- "O95"
bool <- dfDisease$document == drugURI
voir <- subset (dfDisease, bool)

## load romedi :
romedi <- read.table("../../src/main/resources/drugs/romediTermsNormalizedINPINBN.csv",sep="\t",header = F,comment.char = "",quote="")
colnames(romedi) <- c("uri","type","libelle","normal")
romedi$uri <- gsub("http://www.romedi.fr/romedi#","",romedi$uri)
uniqueDocuments <- unique(dfBoth$document)
uniqueDocuments <- data.frame(document = uniqueDocuments)

## add labels : 
# drugs
uniqueDocuments <- merge (uniqueDocuments, romedi, by.x="document",by.y="uri",all.x=T)
uniqueDocuments <- subset (uniqueDocuments, select=c("document","normal"))

# ICD10
diseaseDetected <- read.table("../diseaseDetection/diseasesDetected.csv",sep="\t",comment.char = "")
colnames(diseaseDetected) <- c("candidateTerm","code")
diseaseDetected$code <- substr(x = diseaseDetected$code, 1,3)
## one label per code: 
library(dplyr)
diseaseDetected <- diseaseDetected %>% group_by(code) %>% mutate(n = row_number())
diseaseDetected <- subset (diseaseDetected, n == 1)
diseaseDetected$n <- NULL
diseaseDetected <- unique(diseaseDetected)
uniqueDocuments <- merge(uniqueDocuments, diseaseDetected, by.x="document",by.y="code",all.x = T)
bool1 <- is.na(uniqueDocuments$normal)
bool2 <- is.na(uniqueDocuments$candidateTerm)
uniqueDocuments$libelle <- ifelse(!bool1 & bool2, 
                                  as.character(uniqueDocuments$normal),
                                  ifelse(!bool2 & bool1, as.character(uniqueDocuments$candidateTerm),
                                         as.character(uniqueDocuments$document)))
uniqueDocuments$normal <- NULL
uniqueDocuments$candidateTerm <- NULL

voirDrug <- function(drugLabel){
  bool <- romedi$normal %in% tolower(drugLabel)
  drugURI <- romedi$uri[bool]
  bool <- drugURI %in% uniqueDocuments$document
  drugURI <- drugURI[bool]
  similarityDrugURI <- quanteda::textstat_simil(dfmBoth, selection=c(drugURI), method="cosine")
  voir <- data.frame(document = rownames(similarityDrugURI), 
                     sim = as.numeric(similarityDrugURI))
  voir <- merge(voir, uniqueDocuments, by="document")
  voir <- voir[order(-voir$sim),]
  return(voir)
}

drugLabel <- "rivotril"
voir <- voirDrug(drugLabel)

voir <- voir[,2:3]
voir <- voir[1:10,]
write.table(voir, "voir.csv",sep="\t",col.names = T,row.names = F)

voir2 <- voirDrug(drugLabel)

bool <- grepl("antadys",voir$libelle, ignore.case = T)
any(bool)
voir <- subset (voir, bool)
