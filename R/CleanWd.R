wd<-function (){
  rm(list = ls())
  chemRepTr <- getwd()
  resRchYafExt <- grep("local", chemRepTr, ignore.case = T)
  resRchYafInt <- grep("lb3", chemRepTr, ignore.case = T)
  resRchTourLin <- grep("/home/louis", chemRepTr, ignore.case = T)
  resRchTourWin <- grep("F:/OneDrive", chemRepTr, ignore.case = T)
  
  if (any(resRchYafExt)) {
    setwd("C:/Users/lb3_local/OneDrive/sync/git/")
    chemRepTr
  } else if (any(resRchYafInt)) {
    setwd("C:/Users/lb3/OneDrive/sync/git/")
    chemRepTr
  } else if (any(resRchTourWin)) {
    setwd("F:/OneDrive/sync/git/")
    chemRepTr
  } else if (any(resRchTourLin)) {
    setwd("~/git/")
    chemRepTr
  }
  
  rm(list=ls())
  return(getwd())
}

Unaccent <- function(text) {
  text <- gsub("['`^~\": ]", "_", text)
  text <- gsub("[éèëê]", "e", text)
  text <- gsub("[àäâ]", "a", text)
  return(text)
}