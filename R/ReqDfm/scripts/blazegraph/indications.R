indications <- read.table("indications/theriaque_indication_cim10.csv",sep=",", header=F,quote = "\"",encoding = "ISO-8859-1")

colnames(indications)
head(indications)
colnames(indications) <- c("medicament","indication","code","indicationLong")

BN <- "methotrexate"

getIndication = function(BN){
   bool <- grepl(BN, indications$medicament, ignore.case = T)
   sum(bool)
   if (!any(bool)){
      cat(BN, " non trouvé")
      return(data.frame(code=character(), indication=character(), indicationLong=character()))
   }
   BNindications <- subset (indications, bool,select=c("code","indication"))
   BNindications <- unique(BNindications)
   return(BNindications)
}

voir <- getIndication("quinimax")
unique(voir$indication)
length(levels(indications$indication))