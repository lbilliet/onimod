rm(list = ls())
setwd(dirname(file.choose()))
getwd()
library(tm)

collection <- c(D1 <- "The sky is blue.", D2 <- "The sun is bright.", D3 <- "The sun in the sky is bright.", D4 <- "The sky is red", D5 <- "The stars are shiny.", D6 <- "The sun in the nightsky does not bright.")
testSet <- c("sky", "water", "sun", "stars", "bright", "moon", "shiny", "not", "sea", "land")
dd <- Corpus(VectorSource(collection))#Make a corpus object from a text vector
#Clean the text
dd <- tm_map(dd, stripWhitespace)
dd <- tm_map(dd, tolower)
dd <- tm_map(dd, removePunctuation)
dd <- tm_map(dd, removeWords, stopwords("english"))
dd <- tm_map(dd, stemDocument)
dd <- tm_map(dd, removeNumbers)
dtm <- DocumentTermMatrix(dd, control = list(weighting = weightTfIdf))
as.matrix(dtm)



############# définition fonction création métrique BM25
getBM25 = function(ct, dfReq, k1 = 2, b = 0.75) {
   # term = terme recherché
   # dfReq = dataframe de sortie de la requête sur le terme via elastisearch
   # k1 & b constantes d'ajustement de la formule BM25
   
   ###### attribution variables à manipuler
   nDoc <-
      nrow(dfReq) #total nb de documents (phrases) de la collection
   nDocCt <-
      length(dfReq$ct) #total nb de documents (phrases) contenant le terme recherché
   
   ######définition formules pour TF-IDF
   tf <- sum(dfReq$ct == ct) / nDoc #TF
   idf <- log(nDoc / nDocCt) #IDF
   tfidf <- tf * idf
   
   ##### boucle valable à adapter ?
   # for(term in names(idf)){
   #    tfidf[,term] <- tf[,term] * idf[term]
   # }
   
   ####### def variables BM25
   docLength <- 0
   for (i in 1:nrow(dfReq)) {
      docLength <- docLength + lengthReq$phrase[i]
   }
   meanDocLength <- docLength / nDoc
   
   ####### def formula BM25
   tf25 <-
      (tf * (k1 + 1)) / (tf + k1 * (1 - b + b * docLength / meanDocLength))
   idf25 <-
      log((nDoc - nDocCt + 0.5) / (nDocCt + 0.5))
   bm25 <- tf25 * idf25
   
   return(list(
      $tfidf = df$tfidf,$bm25 = df$bm25,
      dfReq$ct = df$ct
   ))
}