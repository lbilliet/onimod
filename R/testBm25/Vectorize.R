rm(list=ls())
setwd(dirname(file.choose()))
library(elastic)
library(httr)
library(quanteda)
library(dplyr)
library(tidytext)
source("elasticSearchFunction.R")
source(file = "../diseaseDetection/0.chargement.R")
source("usefulFunctions.R")
load("indicationFoundbyDrugURI.rdata")

###### Connection to elasticSearch
elastic::connect(es_host = "172.16.90.22")
host <- "172.16.90.22"
port = "9200"
index <- "sentencect"
type <- "sentencect"

## 
# http://172.16.90.22:9200/sentence/_stats for Number of docs
Ndocs <- 1068711
Ndocs <- 1046480

######################## lemmaTerm (candidateTerm CT)
ctCount <- getCTCount(host = host, port = port, index = index, type = type)
summary(ctCount)
## remove less than 2 char : 
nchars <- nchar(ctCount$ct)
head(nchars)
bool <- nchars < 3
head(bool)
str(bool)
table(bool)
sum(bool)#donne nb TRUE
ctCount <- subset (ctCount, !bool)
ctCount$IDF <- - log (ctCount$frequency / Ndocs)

################################# drugs count : 
drugsCount <- getDrugsCount(host = host, port = port, index = index, type = type)
drugsCount$IDF <- - log (drugsCount$frequency / Ndocs)

############################### disease count : 
diseaseCountCode3 <- getDiseasesCount(host = host, port = port, index = index, type = type)
diseaseCountCode3$code <- substr(x = diseaseCountCode3$code, 1,3) #une lettre et 2 chiffres, pas plus
diseaseCountCode3 <- diseaseCountCode3 %>% group_by(code) %>% mutate (frequencyCode = sum(frequencyCode)) #regroupement des codes raccourcis avec sommes des effectifs
diseaseCountCode3$IDF <- - log (diseaseCountCode3$frequencyCode / Ndocs)
diseaseCountCode3 <- unique(diseaseCountCode3)
summary(diseaseCountCode3)
bool <- diseaseCountCode3$frequencyCode < 5 #initialement fixée à 50 par Sébastien
diseaseCountCode3 <- subset (diseaseCountCode3, !bool)
summary(diseaseCountCode3)

########### disease labels
### as a code can have multiple labels, we take the more frequent label in the data :
diseaseDetected <- read.table("../diseaseDetection/diseasesDetected.csv",sep="\t",comment.char = "")
colnames(diseaseDetected) <- c("candidateTerm","code")
## one label per code: 
library(dplyr)
diseaseDetected <- diseaseDetected %>% group_by(code) %>% mutate(n = row_number())
diseaseDetected <- subset (diseaseDetected, n == 1)
diseaseDetected$n <- NULL
# code 3
diseaseDetected$code3 <- substr(x = diseaseDetected$code, 1,3)


#################################### Vectorize each drugURI code ######################
## drugs labels : 
romedi <- read.table("../diseaseDetection/romediTermsNormalizedINPINBN.csv",sep="\t",header = F,comment.char = "",quote="")
colnames(romedi) <- c("uri","type","libelle","normal")
romedi$uri <- gsub("http://www.romedi.fr/romedi#","",romedi$uri)
dfDrug <- NULL
bool <- drugsCount$frequency < 5 #initialement fixée à 30 par Sébastien
sum(bool)
drugsCount <- subset (drugsCount, !bool)
drugURI <- drugsCount$uri[1]
source("usefulFunctions.R")
## remove drug term in ct : 
# bool <- ctCount$ct %in% romedi$normal
# sum(bool)
# ctCount <- subset (ctCount, !bool)

# drugURIrivotril  <- getURIbyLabel2("rivotril",romedi)
# drugURI <- drugURIrivotril

# drugXURI  <- getURIbyLabel("rivotril",romedi) #original
drugXURI  <- getURIbyLabel("doxylamine",romedi)
drugURI <- drugXURI

getLabelByURI <- function(uri, romedi){
  bool <- romedi$uri == uri
  return(as.character(romedi$normal[bool]))
}

# temp <- temp[1:10,]
# temp$TF <- signif(temp$TF, 2)
# temp$TFIDF <- signif(temp$TFIDF, 2)
# temp$IDF <- signif(temp$IDF, 2)
# write.table(temp, "temp.csv",sep="\t",col.names = T,row.names = F)
for (drugURI in drugsCount$uri){
  print(drugURI)
  print(which(drugsCount$uri == drugURI))
  label <- getLabelByURI(drugURI,romedi)
  if (drugURI == drugXURI){
    temp <- getCTByDrug(drugURI = drugURI,
                        drugsCount = drugsCount, ctCount = ctCount, romedi = romedi, host = host,port = port,
                        index = index,type = type)
  } else {
    temp <- getCTByDrugExcept(drugURI = drugURI,drugURIexcept = drugXURI,
                              drugsCount = drugsCount, ctCount = ctCount, romedi = romedi, host = host,port = port,
                              index = index,type = type)
  }
  if (nrow(temp) == 0){
    next
  }
  ## remove drug label from ct results : 
  bool <- grepl(label, temp$ct)
  temp <- subset(temp, !bool)
  
  ## 
  temp <- subset (temp, select=c("ct","TFIDF"))
  temp$document <- drugURI
  dfDrug <- rbind(dfDrug, temp)
}
# save(dfDrug, file = "dfDrugRivotril.rdata")

# library(xtable)
# print(xtable(temp[1:10,]),include.rownames = F)

diseaseCount <- getDiseasesCount(host = host, port = port, index = index, type = type)
diseaseCount$IDF <- - log (diseaseCount$frequencyCode / Ndocs)
undebug(getDiseasesByDrug)
temp <- getDiseasesByDrug(drugURI,drugsCount,diseaseCount,diseaseDetected, 
                          host, port, index, type)
getLabelByURI(drugURI,romedi)
temp$TF <- signif(temp$TF, 2)
temp$IDF <- signif(temp$IDF, 2)
temp$TFIDF <- signif(temp$TFIDF, 2)
# library(xtable)
# print(xtable(temp[1:10,]),include.rownames = F)



#################################### Vectorize each ICD10 code ###################### 
### DocumentTermMatrices of diseases : 
dfDisease <- NULL
code3 <- "R52"
drugURIexcept <- drugURI
## retrieve all subcodes by a ICD10 with 3 chars
getCodes = function(code3){
  bool <- diseaseDetected$code3 == code3
  codes <- diseaseDetected$code[bool]
  codes <- unique(codes)
  return(codes)
}
for (code3 in diseaseCountCode3$code){
  print(code3)
  print(which(diseaseCountCode3$code == code3))
  codes <- getCodes(code3)
  if (length(codes) == 0){
    cat("disease not detected for code ", code3, "\n")
    next
  }
  # temp <- getCTByDiseaseExcept(drugURI = drugURI, drugURIexcept = drugXURI,
  #                           drugsCount = drugsCount, ctCount = ctCount, romedi = romedi, host = host,port = port,
  #                           index = index,type = type)
  temp <- getCTByDiseaseExcept(code3 = code3, codes = codes, drugURIexcept = drugXURI, diseaseCountCode3 = diseaseCountCode3, ctCount,diseaseDetected = diseaseDetected,
                          host = host,port = port,index = index,type = type)
  if (nrow(temp) == 0){
    next
  }
  temp <- subset (temp, select=c("ct","TFIDF"))
  temp$document <- code3
  dfDisease <- rbind(dfDisease, temp)
}

voir <- subset (dfDisease, TFIDF == max(dfDisease$TFIDF))

# save(dfDisease, file = "dfDiseaseRivotril.rdata")

# dtmDisease <- dfDisease %>%
#   cast_dtm(document, ct, TFIDF,weighting = function(x) {return (x)})
# dfmDisease <- quanteda::as.dfm(dtmDisease)




######################################### Vectorize ATC4 ######################################
library(httr)
library(jsonlite)
drugURI <- "BNj58jf8aqth9bis3rfloeqf278re9l3u7"

getATC4 = function(drugURI){
  drugURI <- paste0("http://www.romedi.fr/romedi#",drugURI)
  results <- httr::GET(url="http://172.16.90.22:8892/romediBlazegraph-0.0.1/GetJSONbyIRI",
                       query= list (IRI = drugURI))
  results <- rawToChar(results$content)
  resultsJson <- jsonlite::fromJSON(results)
  resultsJson$ATC4$`0`$label
}

### add ATC code to each drug URI detected : 
drugURI <- drugsCount$uri[1]

drugsCount$ATC4 <- sapply(drugsCount$uri, function(x){
  tryCatch({
    return(getATC4(x))
  },error = function(e){
    return("")
  })
})

## count ATC : 
tab <- tapply(drugsCount$frequency,as.character(drugsCount$ATC4), sum)
tab <- data.frame(ATC = names(tab), frequency=as.numeric(tab))
tab <- tab[order(-tab$frequency),]
bool <- tab$frequency < 30
dfATC <- NULL
tab <- subset (tab, !bool)
codeATC <- tab$ATC[1]

getDrugsURI = function(codeATC){
  bool <- drugsCount$ATC4 == codeATC
  return(drugsCount$uri[bool])
}


for (codeATC in tab$ATC){
  drugsURI <- getDrugsURI(codeATC = codeATC)
  print(which(codeATC == tab$ATC))
  bool <- drugsURI == drugXURI
  if (any(bool)){
    print("oui !")
    drugsURI <- drugsURI[!bool]
  } 
  temp <- getCTByDrugExcept(drugURI = drugsURI,drugURIexcept = drugXURI,drugsCount = drugsCount, ctCount = ctCount, romedi = romedi, host = host,port = port,
                      index = index,type = type)
  if (nrow(temp) == 0){
    next
  }
  temp <- subset (temp, select=c("ct","TFIDF"))
  temp$document <- codeATC
  dfATC <- rbind(dfATC, temp)
}

# save(dfATC, file = "dfATCRivotril.rdata")
