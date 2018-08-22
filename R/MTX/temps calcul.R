setwd("C:/Users/lb3_local/OneDrive/sync/git/loubill/Domino/R/MTX")
# setwd("F:/OneDrive/sync/git/loubill/Domino/R/MTX")
# setwd("/media/louis/EXCHWINLIN/Gdrive/sync/git/loubill/Domino/R/MTX")
library(devtools)
library(tm)
library(quanteda)
# library(SnowballC)
# library(wordcloud)
# library(topicmodels)
# library("koRpus")
# library("wordVectors")
# library("RNewsflow")
# library("mclustcomp")

# system.time(expr={
######import données
df.mtx<-read.csv2("data/mtx.csv", sep = "\n" ,header=F, quote = "", stringsAsFactors = F, fileEncoding = "UTF8")
df.alt<-read.csv2("data/alt.csv", sep = "\n" ,header=F, quote = "", stringsAsFactors = F, fileEncoding = "UTF8")

#constitution corpus
corpus.mtx = Corpus(VectorSource(df.mtx))
corpus.alt = Corpus(VectorSource(df.alt))

#######consitution matrice documents
#matrice frequence termes
tdm.mtx <- TermDocumentMatrix(corpus.mtx)
tdm.alt <- TermDocumentMatrix(corpus.alt)
#matrice frequence documents
dtm.mtx <- DocumentTermMatrix(corpus.mtx, control = list(weighting = weightTfIdf, stopwords = TRUE))
dtm.alt <- DocumentTermMatrix(corpus.alt, control = list(weighting = weightTfIdf, stopwords = TRUE))

################################
#boucle constitution vecteur concaténation corpus mtx
txt.mtx<-NULL
for (i in 1 : nrow(df.mtx)){
  txt.mtx<-paste0(txt.mtx,df.mtx[i,])
}

vec.mtx<-unlist(strsplit(txt.mtx, split = " "))
vec.mtx<-gsub(" ","",vec.mtx)
# vec.mtx[vec.mtx != ""]
#mettre vecteurs même longueur
vec.mtx<-c(vec.mtx, rep(NA, length(vec.mtx)-length(vec.mtx)))
write.table(vec.mtx, file = "data/stemVecAlt.txt", sep = "\t", row.names = F, quote = F)
write.table(vec.mtx, file = "data/stemVecMtx.txt", sep = "\t", row.names = F, quote = F)

#comparaison via mclustcomp
vecteur1 <- c(0:10)
vecteur2 <- c(11:1)
debug(mclustcomp)
mclustcomp(vecteur1,vecteur2,types="all")
mclustcomp(vec.mtx,vec.mtx, types = "all")#pb ici il faut une boucle pour parcourir chaque vecteur de la df
# })
