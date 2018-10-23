## Get the URI of a romedi drug by its label
getURIbyLabel = function(label, romedi){
  bool <- romedi$normalized == tolower(label)
  drugURI <- romedi$uri[bool]
  print(getIndication(label))
  diseaseByDrug <- getDiseasesByDrug(drugURI = drugURI,drugsCount = drugsCount, diseaseCount = diseaseCount, diseaseDetected = diseaseDetected,
                                     host = host,port = port,index = index, type = type)
  return(list(diseaseByDrug=diseaseByDrug, drugURI=drugURI))
}

## Get Theriaque Indication a drug URI
getIndicationByURI = function(drugURI){
  bool <- romedi$uri == drugURI
  normalized <- romedi$normalized[bool]
  normalized <- normalized[1]
  return(getIndication(normalized))
}

## get ICD10 indications of a brand Name
getIndication = function(BN){
  bool <- grepl(BN, indications$denom, ignore.case = T)
  sum(bool)
  if (!any(bool)){
    cat(BN, " non trouve \n")
    return(data.frame(code=character(), indication=character(), indicationLong=character()))
  }
  BNindications <- subset (indications, bool,select=c("code","indication"))
  BNindications <- unique(BNindications)
  return(BNindications)
}

## A00-A99 => A00-A01 ...
explodeCodeIndic <- function(indic) {
  bool <- grepl("-",indic$code)
  if (!any(bool)){
    return(indic)
  }
  oneCode <- subset (indic, !bool)
  multiCode <- subset (indic, bool)
  dfMultiCode <- NULL
  i <- 1
  for (i in 1:nrow(multiCode)){
    code <- multiCode$code[i]
    ind <- multiCode$indication[i]
    codes <- unlist(strsplit(as.character(code), split="-"))
    if (!length(codes) == 2){
      next
    }
    chapter <- stringr::str_extract(codes, "[A-Z]")
    numbers <- stringr::str_extract(codes, "[0-9]+")
    numbers <- as.numeric(numbers)
    if (any(is.na(numbers))){
      next
    }
    bool <- chapter[1] == chapter[2]
    if (bool) {
      newNumbers <- seq(from=numbers[1], to=numbers[2], by=1)
      newCodes <- paste0(chapter[1], newNumbers)
      ajout <- data.frame(code = newCodes, indication = ind)
      dfMultiCode <- rbind(dfMultiCode, ajout)
    } else {
      newNumbers <- seq(from=numbers[1],to=99, by=1)
      newCodeChapter1 <-  paste0(chapter[1], newNumbers)
      ajout1 <- data.frame(code = newCodeChapter1, indication = ind)
      
      newNumbers <- seq(from=1,to=numbers[2], by=1)
      newCodeChapter2 <-  paste0(chapter[2], newNumbers)
      ajout2 <- data.frame(code = newCodeChapter2, indication = ind)
      dfMultiCode <- rbind(dfMultiCode, ajout2)
      dfMultiCode <- rbind(dfMultiCode, ajout1)
    }
  }
  dfMultiCode <- rbind(dfMultiCode, oneCode)
  return(dfMultiCode)
}