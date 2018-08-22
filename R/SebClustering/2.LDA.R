rm(list=ls())
### chargement du traitement de Treetagger sur les posts IVG : 
load("data/ivg.rdata")
### sur les posts contracpets
load("data/contraception.rdata")

####### pour le rapport :
### terme le plus fréquent par catégorie TreeTagger : 
par(mar=c(4,6,4,4))
tab <- table(ivg$wclass)
bplt <- barplot(tab, horiz=T, las=1, xlim=c(0,max(tab)*1.2))
bool <- ivg$lemma == "<unknown>"
sum(bool)
ivg$lemma[bool] <- ivg$token[bool]
voir <- ivg
test <- tapply(voir$lemma, voir$wclass,function(x){
  tab <- table(x)
  tab <- sort(tab, decreasing=T)
  return(names(tab[1]))
})
test["name"] <- "slt"
lab <- paste (as.numeric(tab), as.character(test),sep="  ")
text(x=tab+10000, y= bplt, labels=lab)

##### fonction permattant de transformer la sortie de TreeTagger
## en objet Corpus de la librairie TM
get_corpus <- function(voir){
  bool <- voir$token == "AAAAAZZZZZ"
  sum(bool)
  inclus <- c("verb","noun","name","adjective")
  bool <- voir$wclass %in% inclus
  voir2 <- subset (voir, bool)
  bool <- voir2$lemma == "<unknown>"
  sum(bool)
  voir2$lemma[bool] <- voir2$token[bool]
  bool <- voir2$token == "AAAAAZZZZZ"
  sum(bool)

  ### enlever les pipe
  voir2$lemma <- lapply(voir2$lemma, function(x){
    x <- unlist(strsplit(x,"\\|"))
      if (length(x) == 1){
    return(x)
  } else {
    return(x[2])
  }
  })
  
  tous <- paste (voir2$lemma,collapse=" ")
  tous <- unlist(strsplit(tous, "AAAAAZZZZZ"))
  length(tous)
  
  ### extraction pour word2vec : 
  # writeLines(tous,"ivg.txt")

  
  ###### analyser ce corpus de termes lemmatisés : 
  library(tm)
  myCorpus <- Corpus(VectorSource(tous))
  myCorpus[[1]]
  myCorpus[[1]]$content
  myCorpus[[2]]$meta
  
  ## fonctions disponibles : 
  getTransformations()
  
  #### fonction pour cleaner les données : 
  removeAccents <- content_transformer(function(x) {
    return (x <- iconv(x, to='ASCII//TRANSLIT'))
  })
  
  #create the toSpace content transformer
  toSpace <- content_transformer(function(x, pattern) {
    return (gsub(pattern, " ", x))
  })
  
  # retirer les accents
  myCorpus <- tm_map(myCorpus, removeAccents)
  myCorpus[[1]]$content
  
  # retirer les mots vides
  motsvides <- c("avoir","etre","faire","bonjour","savoir","merci",
                 "pouvoir","aller","reponse","question",
                 "meme","autre")
  myCorpus <- tm_map(myCorpus, removeWords, motsvides)
  myCorpus[[1]]$content
  
  ## retirer la punctuation
  myCorpus <- tm_map(myCorpus, toSpace, "[[:punct:]]")
  myCorpus <- tm_map(myCorpus,content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus[[1]]$content
  
  ### extraction pour word2vec : 
  # writeLines(tous,"ivg.txt")
  # texte <- unlist(sapply(myCorpus, `[`, "content"))
  # length(texte)
  # writeLines(texte,"ivg.txt")
  return(myCorpus)
}


# myCorpusContraception <- get_corpus(contraception)
myCorpusIVG <- get_corpus(ivg)

## fusion des 2 corpus : 
# fréquence d'apparition de 10 mots à l'infini
# corpus <- append(myCorpusContraception,myCorpusIVG)
dtm <- DocumentTermMatrix(myCorpusIVG,control=list(stemming =F, 
                                              stopwords=F,
                                              minWordLength=1,
                                              removeNumers=T,
                                              removePunctuation=T,
                                              bounds = list(global = c(10,Inf))))

freq <- colSums(as.matrix(dtm))
length(freq)
freq2 <- sort(freq,decreasing = T)
## termes les plus fréquents
freq2[1:100]


############################################### LDA ######################################"
################# Codes de Charrif
library(tm)
library(RTextTools)
library(topicmodels)

# Parametres Gibbs sampling (diagnostics traces MCMC pour établir les bons paramètre)
# Nombre d'iterations 
iter <- 1000
# période de chauffe
burnin <- 100
# thinig  : augmenter décorrélation chaîne
thin <- 5

# Nombre de sujets
k <- 3

# MCMC
# NB : phrase.split(" ").length == 1 --> ne fonctionne pas

#matrix <- create_matrix(Posts[1:146],removeNumbers=TRUE)
ldaOut <-LDA(dtm,k, method = "Gibbs", 
             control=list(burnin = burnin, 
                          iter = iter, thin=thin))

# n termes les plus probable dans chaque sujet   
n <- 20
ldaOut.terms <- as.matrix(terms(ldaOut,n))
ldaOut.terms
topicProbabilities <- as.data.frame(ldaOut@gamma)
bool <- topicProbabilities$V1 > topicProbabilities$V2
### Classifier 
topicProbabilities$topic <- ifelse(bool, "Topic1","Topic2")
topicProbabilities$categorie <- c(rep("Contraception",length(myCorpusContraception)),
                                  rep("IVG",length(myCorpusIVG)))

ftable(topicProbabilities$topic, topicProbabilities$categorie)


################################# Visualisation ###############" 
library("slam")
summary(col_sums(dtm))
freq <- colSums(as.matrix(dtm))
freq <- sort(freq)
freq <- data.frame(lem=names(freq), frequence=as.numeric(freq))
freq <- freq[order(-freq$frequence),]
head(freq)
findAssocs(dtm,"pilule",0.3)
### wordcloud permettant de sortie un wordcloud à partir d'un corpus :
wordcloudFromCorpus <- function(corpus, nwords){
  library(wordcloud)
  dtm <- DocumentTermMatrix(corpus,control=list(stemming =F, 
                                                stopwords=F,
                                                minWordLength=1,
                                                removeNumers=T,
                                                removePunctuation=T,
                                                bounds = list(global = c(10,Inf))))
  set.seed(42)
  #limit words by specifying min frequency
  freq <- colSums(as.matrix(dtm))
  wordcloud(names(freq),freq,max.words = nwords)
}
wordcloudFromCorpus(myCorpusIVG, 20)
wordcloudFromCorpus(myCorpusContraception, 20)

### pb de lemmatisation : 
voir2 <- subset (ivg, lemma=="enceindre")