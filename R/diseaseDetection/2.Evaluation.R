rm(list=ls())
setwd(dirname(file.choose()))
getwd()
library(xtable)
library(stringr)
source("0.chargement.R")
source("usefulFunctions.R")
source("elasticSearchFunction.R")
load("indicationFoundbyDrugURI.rdata")
boxplot(indicationFoundbyDrugURI$position)
boxplot(indicationFoundbyDrugURI$sumTF)
fivenum(indicationFoundbyDrugURI$position)
fivenum(indicationFoundbyDrugURI$sumTF)

signal <- subset (indicationFoundbyDrugURI, position > 5)

## add the label : 
colnames(signal)
signal <- merge (signal, romedi, by.x = "drugURI", by.y="uri")
signal <- signal[order(signal$position),]

## 
drugsCount30 <- subset (drugsCount, frequency>30)
bool <- signal$drugURI %in% drugsCount30$uri
sum(bool)
signal2 <- subset(signal, bool)

###### see to associated disease : 
log(3503)
######## pick a label : 

#test rch ATU / RTU
AtuRtu <- read.csv2("data/ATU_RTU.csv",header = T,encoding = "UTF-8") #si besoin de load, mais normalement dans source "0.chargement.R"

##############export DF LaTEX
# diseasesByDrugOrd<-diseasesByDrug[with(diseasesByDrug, order(diseasesByDrug$TFIDF)),]
diseasesByDrugOrd<-diseasesByDrug[,c(1,8,7,2,3,4,5,6)]
# diseasesByDrugOrd<-diseasesByDrugOrd[with(diseasesByDrugOrd, order(diseasesByDrugOrd$TFIDF)),]
print(xtable(diseaseCount[1:15,]), include.rownames=FALSE)
print(xtable(diseaseDetected[1:15,]), include.rownames=FALSE)
print(xtable(diseaseDetectedDistinct[1:15,]), include.rownames=FALSE)
print(xtable(diseaseByDrug[1:15,c(1,8,7,4,6)]), include.rownames=FALSE)
print(xtable(diseasesByDrug[1:15,c(1,8,7,4,6)]), include.rownames=FALSE)
print(xtable(diseasesByDrugOrd[1:15,]), include.rownames=FALSE)
print(xtable(drugsCount[1:15,]), include.rownames=FALSE)
print(xtable(drugsCount30[1:15,]), include.rownames=FALSE)
print(xtable(indicationFoundbyDrugURI[1:15,]), include.rownames=FALSE)
print(xtable(romedi[1:15,]), include.rownames=FALSE)
print(xtable(signal[1:15,]), include.rownames=FALSE)
print(xtable(signal2[1:15,]), include.rownames=FALSE)

print(xtable(indications[c(11,12,17,18,19,20,21,22),c(1,3,4)]), include.rownames=FALSE)

# => Theralene est indique dans les épisodes d'insomnies !
getPostsMedoc <- function(medoc, numCode = 1){
  results <- getURIbyLabel(medoc,romedi)
  diseaseByDrug <- results$diseaseByDrug
  drugURIs <- results$drugURI
  # drugURI
  diseaseByDrug$candidateTerm[1:14]
  code <- diseaseByDrug$code[numCode] ## 4 => numero du terme
  code
  i <- 1
  posts <- NULL
  while (is.null(posts) & i < (length(drugURIs) + 1)){
    # print(paste("Valeur de i (pour drugURI):",i, sep = " "))
    drugURI <- drugURIs[i]
    posts <- getDrugAndDisease(drugURI, code,host = host,
                               port = port,index = index,type = type)
    i <- i + 1
  }
  print(diseaseByDrug$candidateTerm[1:10])
  bool_ATU<-unique(str_detect(as.character(unlist(AtuRtu)), fixed(paste("\"",medoc,"\""), ignore_case = TRUE )))
  print(paste("ATU/RTU ?", bool_ATU, sep=" "))
  posts$sentence
}
getPostsMedoc("rivotril", numCode = 1 )

# medoc<-"alpelisib"
# unlistAtuRtu <-  unlist(AtuRtu)
# any(grepl(medoc,unlistAtuRtu,ignore.case = T))
# 
# unique(str_detect(as.character(unlist(AtuRtu)), fixed(paste("\"",medoc,"\"", sep = ""), ignore_case = TRUE )))
# unique(str_detect(as.character(unlist(AtuRtu)), fixed("alpelisib", ignore_case = TRUE )))#ALPELISIB
# ignore.case(medoc)

### all the terms of a code : 
voir <- subset(diseaseDetectedDistinct, code == "M701")
voir[1:20,]
