# install.packages("jsonlite")
# install.packages("udpipe")
library(udpipe)
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
romedi <- read.table("romediTermsNormalizedINPINBN.csv",sep="\t",header = F,comment.char = "",quote="", encoding = "UTF-8")
colnames(romedi) <- c("uri","type","libelle","normal")
romedi$uri <- gsub("http://www.romedi.fr/romedi#","",romedi$uri)

#### drugs count : 
drugsCount <- getDrugsCount(host = host, port = port, index = index, type = type)
# initialement => IDF utilisé, voire perf avec weight BM25 Okapi
drugsCount$IDF <- - log (drugsCount$frequency / Ndocs)
drugsCount$IDF25 <- - log (drugsCount$frequency / Ndocs)
drugsCount$BM25 <- 

### disease count : 
diseaseCount <- getDiseasesCount(host = host, port = port, index = index, type = type)
## keep only ICD10 with 3 codes
diseaseCount <- diseaseCount %>% group_by(code) %>% mutate (frequencyCode = sum(frequencyCode))
diseaseCount$IDF <- - log (diseaseCount$frequencyCode / Ndocs)
diseaseCount <- unique(diseaseCount)

########### disease labels
### as a code can have multiple labels, we take the more frequent label in the data :
diseaseDetected <- read.table("diseasesDetected.csv",sep="\t",comment.char = "", encoding = "UTF-8")
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
indications  <- read.table("theriaque_indication_cim10.csv",sep=",", encoding = "UTF-8")
colnames(indications) <- c("denom","indication","code","indicationLong")

############ load df infos ATU/RTU
# AtuRtu <- read.table("data/ATU_RTU.csv",sep=";",header = F,comment.char = "",quote="", encoding = "UTF-8")
AtuRtu <- read.csv("data/ATU_RTU.csv",header = F,encoding = "UTF-8")
