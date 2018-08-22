rm(list=ls())
# source("CleanWd.R")
# wd()
# setwd(dirname(file.choose()))
getwd()
source("0.chargement.R")
source("usefulFunctions.R")
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
# rm(diseasesByDrug)## retirer

######## pick a label : 
# => Theralene est indique dans les épisodes d'insomnies !
getPostsMedoc <- function(medoc){
  results <- getURIbyLabel(medoc,romedi)
  diseaseByDrug <- results$diseaseByDrug
  drugURIs <- results$drugURI
  # drugURI
  diseaseByDrug$candidateTerm[1:14]
  code <- diseaseByDrug$code[1] ## 4 => numero du terme
  code
  i <- 1
  posts <- NULL
  while (is.null(posts) & i < (length(drugURIs) + 1)){
    print(i)
    drugURI <- drugURIs[i]
    posts <- getDrugAndDisease(drugURI, code,host = host,
                               port = port,index = index,type = type)
    i <- i + 1
  }
  print(diseaseByDrug$candidateTerm[1:10])
  posts$sentence
}
getPostsMedoc("cytotec")

# Encoding(posts$sentence) <- "UTF-8"
# iconv(posts$sentence, "UTF-8", "ISO-8859-1")
# iconvlist()

### all the terms of a code : 
voir <- subset(diseaseDetectedDistinct, code == "E87")
voir[1:20,]
# xlsx::write.xlsx(signal2,file="signal2.xlsx", sheetName = "signal2")