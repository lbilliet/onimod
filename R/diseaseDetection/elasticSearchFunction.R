#' @param body a json query to send to elasticsearch 6.0.1
sendESquery <- function(host, port, index, type, body){
  url <- paste0("http://",host, ":", port, "/",index,"/",type,"/_search?")
  response <- httr::POST(url = url,
                         body = body,
                         httr::content_type("application/json"))
  docs <- rawToChar(response$content)
  docs <- jsonlite::fromJSON(docs)
  # cat("it tooks ", docs$took, " ms\n")
  return(docs)
}

#' @description count the lemmaTerms
getCTCount = function(host, port, index, type){
  body <- 
    '{
        "aggs": {
          "uri": {
            "terms": {
               "field": "ct",
              "size": 1000000,
              "min_doc_count": 5
                    }
                }
              }
  }
  '
  resultsJson <- sendESquery(host, port, index, type, body)
  drugsCount <- resultsJson$aggregations$uri$buckets
  colnames(drugsCount) <- c("ct","frequency")
  return(drugsCount)
}


#' @description count the drugs (romedi URI) indexed in ElasticSearch. Return a romedi URI with its frequency
getDrugsCount = function(host, port, index, type){
  body <- 
    '{
        "aggs": {
          "uri": {
            "terms": {
               "field": "codeDrug",
              "size": 100000
                    }
                }
              }
  }
  '
  resultsJson <- sendESquery(host, port, index, type, body)
  drugsCount <- resultsJson$aggregations$uri$buckets
  colnames(drugsCount) <- c("uri","frequency")
  return(drugsCount)
}

#' @description count the disease (ICD10 code) indexed in ElasticSearch. Return a ICD10 with its frequency
getDiseasesCount = function(host, port, index, type){
  body <- 
    '
      {
        "aggs": {
          "uri": {
            "terms": {
            "field": "codeDisease",
            "size": 100000
                    }
                }
              }           
      }
  '
  resultsJson <- sendESquery(host, port, index, type, body)
  diseaseCount <- resultsJson$aggregations$uri$buckets
  colnames(diseaseCount) <- c("code","frequencyCode")
  return(diseaseCount)
}

#' @description Given a drug, retrieve the most frequent (TF) and specific (IDF) disease (ICD10 codes) associated
#' @param drugURI : a romedi URI
#' @param drugsCount : getDrugsCount()
#' @param diseaseCount : getDiseasesCount()
#' @param diseaseDetected : a data.frame with 2 columns : code and label/CandidateTerm 

getDiseasesByDrug = function(drugURI, drugsCount, diseaseCount, diseaseDetected, host, port, index, type){
  drugURIreq <- paste0("\"",drugURI,"\"")
  drugURIreq <- paste0(drugURIreq, collapse=",")
  body <- paste0(
    '
      {
        "query":{
            "terms":{
              "codeDrug":[',drugURIreq,']
                  }
                },
        "aggs": {
          "uri": {
            "terms": {
            "field": "codeDisease",
            "size": 100000
                    }
                }
              }
      }
  ')
  # cat(body)
  resultsJson <- sendESquery(host, port, index, type, body)
  drugsDisease <- resultsJson$aggregations$uri$buckets
  colnames(drugsDisease) <- c("code","frequencyCodeDrug")
  ## adding frequency of the drugURI to have the code frequency for this drug (TF : ) in this context
  bool <- drugsCount$uri %in% drugURI
  freqURI <- sum(drugsCount$frequency[bool])
  drugsDisease$frequencyDrug <- freqURI
  drugsDisease$TF <- drugsDisease$frequencyCodeDrug / drugsDisease$frequencyDrug
  
  ### adding the IDF of the ICD10 code : 
  drugsDisease <- merge (drugsDisease, diseaseCount, by="code")
  drugsDisease$TFIDF <- drugsDisease$TF * drugsDisease$IDF
  
  ### adding the label of the drug : 
  drugsDisease <- merge (drugsDisease, diseaseDetected, by="code")
  drugsDisease <- drugsDisease[order(-drugsDisease$TFIDF),]
  return(drugsDisease)
}

#' @description Given a drug, retrieve the most frequent (TF) and specific (IDF) lemmaTerm associated
#' @param drugURI : a romedi URI
#' @param drugsCount : getDrugsCount()
#' @param ctCount : getCTCount()

getCTByDrug = function(drugURI, drugsCount, ctCount, romedi, host, port, index, type){
  drugURIreq <- paste0("\"",drugURI,"\"")
  drugURIreq <- paste0(drugURIreq, collapse=",")
  body <- paste0(
    '
    {
    "query":{
    "terms":{
    "codeDrug":[',drugURIreq,']
    }
    },
    "aggs": {
    "uri": {
    "terms": {
    "field": "ct",
    "size": 5000
    }
    }
    }
    }
    ')
  # cat(body)
  resultsJson <- sendESquery(host, port, index, type, body)
  drugsDisease <- resultsJson$aggregations$uri$buckets
  colnames(drugsDisease) <- c("ct","frequencyCTDrug")
  ## adding frequency of the drugURI to have the code frequency for this drug (TF : ) in this context
  bool <- drugsCount$uri %in% drugURI
  freqURI <- sum(drugsCount$frequency[bool])
  drugsDisease$frequencyDrug <- freqURI
  drugsDisease$TF <- drugsDisease$frequencyCTDrug / drugsDisease$frequencyDrug
  
  ### adding the IDF of the ICD10 code : 
  drugsDisease <- merge (drugsDisease, ctCount, by="ct")
  drugsDisease$TFIDF <- drugsDisease$TF * drugsDisease$IDF
  
  bool <- drugsDisease$TF < 0.001
  drugsDisease <- subset (drugsDisease, !bool)
  bool <- romedi$uri %in% drugURI
  normal <- romedi$normal[bool]
  bool <- drugsDisease$ct %in% normal
  drugsDisease <- subset (drugsDisease,!bool)
  drugsDisease <- drugsDisease[order(-drugsDisease$TFIDF),]
  return(drugsDisease)
}

#' @param code a ICD 10 code
#' @param diseaseCount : diseaseCount() and count ICD10 codes with 3 chars
getCTByDisease = function(code3, codes, diseaseCountCode3, ctCount, diseaseDetected, host, port, index, type){
  codereq <- paste0("\"",codes,"\"")
  codereq <- paste0(codereq, collapse=",")
  body <- paste0(
    '
    {
      "query":{
        "terms":{
        "codeDisease":[',codereq,']
                }
              },
      "aggs": {
        "uri": {
        "terms": {
          "field": "ct",
          "size": 5000
                }
              }
            } 
    }
    ')
  resultsJson <- sendESquery(host, port, index, type, body)
  diseaseDrugs <- resultsJson$aggregations$uri$buckets
  if (length(diseaseDrugs) == 0){
    return(data.frame(ct=character(), TFIDF=numeric()))
  }
  colnames(diseaseDrugs) <- c("ct","frequencyCTCode")
  
  ## adding frequency of the ICD10 code to have the code frequency for this disease (TF ) in this context
  bool <- diseaseCountCode3$code %in% code3
  freqCode <- diseaseCountCode3$frequencyCode[bool]
  diseaseDrugs$freqCode <- freqCode
  diseaseDrugs$TF <- diseaseDrugs$frequencyCTCode / diseaseDrugs$freqCode
  
  ### adding the IDF of the drug : 
  diseaseDrugs <- merge (diseaseDrugs, ctCount, by="ct")
  diseaseDrugs$TFIDF <- diseaseDrugs$TF * diseaseDrugs$IDF
  diseaseDrugs <- diseaseDrugs[order(-diseaseDrugs$TFIDF),]
  
  ## remove lemmaTerms to detect the drug : 
  # TODO : getLemmaTerms to remove 
  bool <- diseaseDrugs$TF > 0.5
  # bool <- diseaseDetected$code3 %in% code3
  # lemmaTerm <- diseaseDetected$candidateTerm[bool]
  # bool <- diseaseDrugs$ct %in% lemmaTerm
  diseaseDrugs <- subset (diseaseDrugs, !bool)
  bool <- diseaseDrugs$TF < 0.001
  diseaseDrugs <- subset (diseaseDrugs, !bool)
  return(diseaseDrugs)
}

#' @param code a ICD 10 code
#' @param drugsCount : drugsCount()
#' @param diseaseCount : diseaseCount()
#' @param romedi : labels of drugsURI
getDrugByDisease = function(code, drugsCount, diseaseCount, romedi, host, port, index, type){
  codereq <- paste0("\"",code,"\"")
  codereq <- paste0(codereq, collapse=",")
  body <- paste0(
    '
    {
      "query":{
        "terms":{
        "codeDisease":[',codereq,']
                }
              },
      "aggs": {
        "uri": {
        "terms": {
          "field": "codeDrug",
          "size": 5000
                }
              }
            } 
    }
    ')
  resultsJson <- sendESquery(host, port, index, type, body)
  diseaseDrugs <- resultsJson$aggregations$uri$buckets
  colnames(diseaseDrugs) <- c("uri","frequencyDrugCode")
  
  ## adding frequency of the ICD10 code to have the code frequency for this disease (TF ) in this context
  bool <- diseaseCount$code == code
  freqCode <- diseaseCount$frequencyCode[bool]
  diseaseDrugs$freqCode <- freqCode
  diseaseDrugs$TF <- diseaseDrugs$frequencyDrugCode / diseaseDrugs$freqCode
  
  ### adding the IDF of the drug : 
  diseaseDrugs <- merge (diseaseDrugs, drugsCount, by="uri")
  diseaseDrugs$TFIDF <- diseaseDrugs$TF * diseaseDrugs$IDF
  diseaseDrugs <- merge (diseaseDrugs, romedi, by="uri")
  diseaseDrugs <- diseaseDrugs[order(-diseaseDrugs$TFIDF),]
  return(diseaseDrugs)
}


#' @param drugURI : a romedi URI
#' @param code a ICD 10 code
getDrugAndDisease = function(drugURI, code, host, port, index, type){
  body <- paste0('
{
  "query":{
      "bool" : {
        "must" : [
              {
                "term":{
                "codeDrug":"',drugURI,'"
                      }
              },
              {
                "term":{
                "codeDisease":"',code,'"
                      }
              }
                ]
              }
          }
}'
  )
  resultsJson <- sendESquery(host, port, index, type, body)
  return(resultsJson$hits$hits$`_source`)
}



###### stats : 
getStats <- function(){
  body <- '{
"aggs": {
  "test": {
    "stats": {
      "script": "doc[\'codeDrug\'].values.length"
    }
  }
}
}
'
sendESquery(host,port,index,type,body)
}





############# TODO 
## forum, section and sous-section : 
# getSectionByDrug = function(drugURI){
#   body <- paste0(
#     '
#     {
#     "query":{
#     "term":{
#     "codeDrug":"',drugURI,'"
#     }
#     },
#     "aggs": {
#     "uri": {
#     "terms": {
#     "field": "secondSection",
#     "size": 100000
#     }
#     }
#     }
#     }
#     ')
#   resultsJson <- sendESquery(body)
#   results <- resultsJson$aggregations$uri$buckets
#   return(results)
# }