source("CleanWd.R")
wd()
setwd("loubill/Domino/R/DiseaseDetection")
source("0.chargement.R")

### voir des posts avec un drugURI et un code : 
# seroplex et reflux : 
drugURI <- "BNj58jf8aqth9bis3rfloeqf278re9l3u7"
code <- "K21"
bool <- grepl("K21",diseaseDetected$code)
voir <- subset (diseaseDetected, bool)
voir <- unique(voir)
code <- unique(voir$code)
voir <- getDrugAndDisease(drugURI, code,host = host,port = port,index = index,type = type)

# getDrugByDisease(code, drugsCount,diseaseCount,romedi,host = host,port = port,index = index,type = type)

#############################
getIndication("gaviscon")

# voir <- subset (drugsCount2, uri == drugURI)
##################### some useful function

## adding the label to drug count : 
drugsCount2 <- merge (drugsCount, romedi, by="uri")
drugsCount2 <- drugsCount2[order(-drugsCount2$frequency),]
label <- as.character(drugsCount2$normalized[2])
row <- which(drugURI == drugsCount2$uri)
indicationFoundbyDrugURI <- NULL
row <- 2083
for (row in 1:nrow(drugsCount2)){
  print(row)
  label <- drugsCount2$normalized[row]
  # print(label)
  drugURI <- drugsCount2$uri[row]
  indic <- getIndication(as.character(label))
  # print(indic)
  if (nrow(indic) == 0 ){
    next
  }
  indic <- try({
    indic <- explodeCodeIndic(indic)
  })
  try({
    debug(getDiseasesByDrug)
    diseaseByDrug <- getDiseasesByDrug(drugURI = drugURI,drugsCount = drugsCount, diseaseCount = diseaseCount, diseaseDetected = diseaseDetected,
                                       host = host,port = port,index = index, type = type)
    diseaseByDrug$code3 <- substr(diseaseByDrug$code,1,3)
    bool <- diseaseByDrug$code3 %in% indic$code
    if (any(bool)){
      diseaseByDrug$indication <- bool
      position <- min(which(diseaseByDrug$indication))
      TF <- diseaseByDrug$TF[position]
      sumTF <- sum(diseaseByDrug$TF[bool])
      ajout <- data.frame(drugURI = drugURI, position = position, TF = TF, row = row, found=T,sumTF = sumTF)
      indicationFoundbyDrugURI <- rbind(indicationFoundbyDrugURI, ajout)
    } else {
      position <- nrow(diseaseByDrug) + 1
      ajout <- data.frame(drugURI = drugURI, position = position, TF = 0, row = row, found=F, sumTF=0)
      indicationFoundbyDrugURI <- rbind(indicationFoundbyDrugURI, ajout)
    }
  })
}

# save(indicationFoundbyDrugURI, file="indicationFoundbyDrugURI.rdata")
# boxplot(indicationFoundbyDrugURI$sumTF)


## THAIS a retiré ! 


## search number of patient for a drug : 
body <- '{
"stored_fields": "patient_num",
  "query":{
"nested": {
"path" : "drugs",
"query": {
"terms":{
"drugs.uri":["BNcjkc2f3kuf6hing2m9j564uu78nnsbvc",
"BNc6f6ufbbf535f5a2ag7eole0n7mrsn5r"]
}
}
}

},
  "aggs" : {
    "patient" : {
"cardinality" : { "field" : "patient_num" }
}
}
}
'
response <- httr::POST(url="sim1dev:9200/drugrhumato/freeText/_search?",
                       body = body )
docs <- rawToChar(response$content)
docs
docs <- jsonlite::fromJSON(docs)
voir <- (docs$hits$hits)
docs$aggregations$patient$value

##### value_count : total count
body <- '{
  "aggs" : {
    "patient" : {
        "cardinality" : { "field" : "patient_num" }
    }
}
}
'
response <- httr::POST(url="sim1dev:9200/drugrhumato/freeText/_search?size=0",
                       body = body)
patientCount <- rawToChar(response$content)
patientCount

############## number of drugs associated to a list of drugs
body <- 
'
{
  "query":{
      "nested": {
      "path" : "drugs",
      "query": {
      "terms":{
      "drugs.uri":["BNcjkc2f3kuf6hing2m9j564uu78nnsbvc",
                    "BNc6f6ufbbf535f5a2ag7eole0n7mrsn5r"]
              }
              }
                }
          },
  "aggs": {
    "Nesting": {
      "nested": {
        "path": "drugs"
       },
      "aggs": {
        "uri": {
          "terms": {
            "field": "drugs.uri",
          "size": 1000
          }
    }
  }
  }
  }
  }
'

  
#### 
  ############## 
  body <- 
    '
  {
  "aggs": {
    "Nesting": {
      "nested": {
        "path": "drugs"
                },
      "aggs": {
      "uri": {
      "terms": {
      "field": "drugs.uri",
    "size": 1000
              }
            }
  }
  }
  }
  }
  '
   
## verifier ce résultat
response <- httr::POST(url="sim1dev:9200/drugrhumato/freeText/_search?size=0",
                         body = body)
aggregateURI <- rawToChar(response$content)
library(jsonlite)
result <- jsonlite::fromJSON(aggregateURI)  
aggs <-  result$aggregations$Nesting$uri$buckets

library(SPARQL)
query <- "prefix frnorm: <http://www.erias.fr/BDPMnormalized#>
SELECT ?uri ?type ?label WHERE {
?uri a ?type ;
  rdfs:label ?label .
VALUES ?type {frnorm:BN frnorm:IN}
}"

url <- "http://sim1dev:8889/bigdata/namespace/BDPM2/sparql"
alignement <- SPARQL::SPARQL(url = url, query = query,
                             ns = c('','http://www.erias.fr/BDPMnormalized#'))
medocs <- alignement$results
medocs[] <- lapply(medocs, function(x){
  gsub(":","",x)
})
aggs2 <- merge (aggs, medocs, by.x="key",by.y="uri")
aggs2 <- aggs2[order(-aggs2$doc_count),]
colnames(medocs)
Encoding(aggs2$label) <- "ISO-8859-1"
# aggs2$key
# voir <- subset (aggs2, key == "BNc6f6ufbbf535f5a2ag7eole0n7mrsn5r")

### 
## search number of patient for a drug : 
body <- '{
  "stored_fields": "patient_num",
    "query":{
      "nested": {
      "path" : "drugs",
        "query": {
      "terms":{
        "drugs.uri":["BNcjkc2f3kuf6hing2m9j564uu78nnsbvc",
        "BNc6f6ufbbf535f5a2ag7eole0n7mrsn5r"]
              }
            }
          }
},
"aggs" : {
"patient" : {
"cardinality" : { "field" : "patient_num" }
}
}
}
'



uriVector <- c("BNcjkc2f3kuf6hing2m9j564uu78nnsbvc","BNc6f6ufbbf535f5a2ag7eole0n7mrsn5r")
body <- getBody(uriVector = uriVector, boolMust = FALSE)
cat(body)
response <- httr::POST(url="sim1dev:9200/drugrhumato/freeText/_search?size=0",
                       body = body )
aggregateURI <- rawToChar(response$content)
aggregateURI
library(jsonlite)
result <- jsonlite::fromJSON(aggregateURI)  
aggs <-  result$aggregations$Nesting$uri$buckets










### debug function : 
uriVector <- c("substance101106","BNek6ptr7bj42qjfnnlbqq6icb4enoeb40")
boolMust <- T
getPatientCountDrugsURI = function(uriVector, boolMust){
  ## rather complicated body request
  ## privateFunction
  private_getBody = function(uriVector, boolMust){
    if (boolMust){
      boolean <- "must"
    } else {
      boolean <- "should"
    }
    
    getSearchTermJson = function(uriVector){
      getJsonTerm = function(uri){
        jsonSearchTerm <- paste0('{
                                 "term": {
                                 "drugs.uri": "',uri,'"
                                 }
      }')
          }
      jsonTerms <- sapply(uriVector, function(x) getJsonTerm(x))
      jsonTerms <- paste(jsonTerms, collapse=",")
      return(jsonTerms)
  }
    searchTermsJson <- getSearchTermJson(uriVector)
    body <- paste0('
                   {
                   "query": {
                   "bool": {
                   "',boolean,'": [',searchTermsJson,']
                      }
                   },
                   "aggs" : {
                   "patient" : {
                   "cardinality" : { "field" : "patient_num" }
                        }
                      }
                   }
                   ')
}
  body  <- private_getBody(uriVector, boolMust)
  cat(body)
  resultsJson <- sendESquery(body)
  patientCount <- resultsJson$aggregations$patient$value
  return(patientCount)
}


sendESquery = function(body){
  urlES <- "sim1dev:9200/drugrhumato/freeText/_search?size=0"
  response <- httr::POST(url = urlES,
                         body = body)
  results <- rawToChar(response$content)
  print(results)
  resultsJson <- jsonlite::fromJSON(results)
  return(resultsJson)
}

results <- getPatientCountDrugsURI(uriVector = uriVector, boolMust = boolMust)

getDrugsCount = function(){
  body <- 
    '
  {
  "aggs": {
  "uri": {
  "terms": {
  "field": "drugs.uri",
  "size": 100000
  }
  }
  }
  }
  '
  resultsJson <- sendESquery(body)
  drugsCount <- resultsJson$aggregations$uri$buckets
  return(drugsCount)
}
getDrugsCount()
closeBulk()
print(drugsCount)





####### count number of drugs per docs :
body <- '{
"aggs": {
  "test": {
    "stats": {
      "script": "doc[\'drugs.uri\'].values.length"
    }
  }
}
}
'

## count number of patients : 
body <- '{
  "aggs": {
    "patients": {
      "cardinality": {
        "field": "patient_num"
    }
  }
}
}'

## count number of patients : 
body <- '{
    "aggs": {
      "patients": {
        "terms": {
          "field": "drugs.uri"
        },
        "aggs" : {
    "patients": {
      "cardinality": {
          "field": "patient_num"
                } 
            }
        }
      }
    }
}'