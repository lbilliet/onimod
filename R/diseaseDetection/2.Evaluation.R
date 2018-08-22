rm(list=ls())
source("CleanWd.R")
wd()
# setwd("..//diseaseDetection")
setwd(dirname(file.choose()))
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
diseasesByDrug <- NULL ## retirer

######## pick a label : 
# => Theralene est indique dans les épisodes d'insomnies !
results <- getURIbyLabel("provames",romedi)
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
posts$sentence[1:10]

### all the terms of a code : 
voir <- subset(diseaseDetectedDistinct, code == "Z922")
voir[1:20,]
# xlsx::write.xlsx(signal2,file="signal2.xlsx", sheetName = "signal2")
