# install.packages("jsonlite")
library(elastic)
library(httr)
library(quanteda)
library(dplyr)
library(tidytext)
source("elasticSearchFunction.R")
###### Connection to elasticSearch
elastic::connect(es_host = "172.16.90.22")
host <- "172.16.90.22"
port = "9200"
index <- "sentencect"
type <- "sentencect"

# elastic::index_delete(index)
# elastic::index_delete("sentencect")
# elastic::index_exists(index)

## 
# http://172.16.90.22:9200/sentence/_stats for Number of docs
Ndocs <- 1068711 ## first index
Ndocs <- 1046480


## drugs labels : 
romedi <- read.table("romediTermsNormalizedINPINBN.csv",sep="\t",header = F,comment.char = "",quote="")
colnames(romedi) <- c("uri","type","libelle","normal")
romedi$uri <- gsub("http://www.romedi.fr/romedi#","",romedi$uri)

#### drugs count : 
drugsCount <- getDrugsCount(host = host, port = port, index = index, type = type)
drugsCount$IDF <- - log (drugsCount$frequency / Ndocs)

### disease count : 
diseaseCount <- getDiseasesCount(host = host, port = port, index = index, type = type)
## keep only ICD10 with 3 codes
diseaseCount <- diseaseCount %>% group_by(code) %>% mutate (frequencyCode = sum(frequencyCode))
diseaseCount$IDF <- - log (diseaseCount$frequencyCode / Ndocs)
diseaseCount <- unique(diseaseCount)

########### disease labels
### as a code can have multiple labels, we take the more frequent label in the data :
diseaseDetected <- read.table("diseasesDetected.csv",sep="\t",comment.char = "")
colnames(diseaseDetected) <- c("candidateTerm","code")
diseaseDetectedDistinct <- unique(diseaseDetected)
## one label per code: 
diseaseDetected <- diseaseDetected %>% group_by(code) %>% mutate(n = row_number())
diseaseDetected <- subset (diseaseDetected, n == 1)
diseaseDetected$n <- NULL
# code 3
diseaseDetected$code3 <- substr(x = diseaseDetected$code, 1,3)

## test 
drugURI <- "BNmvjqk095uj1aq6ognukn9npgm4rltqi8"
diseasesByDrug <- getDiseasesByDrug(drugURI = drugURI,drugsCount = drugsCount, diseaseCount = diseaseCount, diseaseDetected = diseaseDetected,
                                    host = host,port = port,index = index, type = type)

#############  Comparison with a list of ICD10 indications by Theriaque 
indications  <- read.table("theriaque_indication_cim10.csv",sep=",")
colnames(indications) <- c("denom","indication","code","indicationLong")
