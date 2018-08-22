setwd("C:/Users/lb3_local/OneDrive/sync/git/loubill/Domino/R/MTX")
# setwd("F:/OneDrive/sync/git/loubill/Domino/R/MTX")
# setwd("/media/louis/EXCHWINLIN/Gdrive/sync/git/loubill/Domino/R/MTX")
library(tm)
library(quanteda)
library(stringr)
library(readtext)

######import données
####https://docs.quanteda.io/articles/pkgdown/replication/digital-humanities.html
vec.mtx<-scan(file = "data/mtx.csv", sep="\t", what = character(), quote = "")
vec1.mtx<-scan(file = "data/mtx1.csv", sep="\t", what = character(), quote = "")
vec2.mtx<-scan(file = "data/mtx2.csv", sep="\t", what = character(), quote = "")
vec3.mtx<-scan(file = "data/mtx3.csv", sep="\t", what = character(), quote = "")
vec.concat.mtx<-c(vec1.mtx, vec2.mtx, vec3.mtx)
summary(vec.concat.mtx)
vec.concat.mtx
summary(vec.mtx)#pourquoi 50 ici ?
head(unlist(vec.mtx, recursive = FALSE, use.names = FALSE))

#reprocessing tokens
tok.mtx<-tokens(vec.concat.mtx) 
dfm.mtx<-dfm(tok.mtx, remove_punct=TRUE, remove_numbers = TRUE, tolower = TRUE, verbose = TRUE)
dfm.mtx
dfm.mtx <- dfm_remove(dfm.mtx, "[0-9]", valuetype = "regex", verbose = TRUE)
dfm.mtx
ntoken(dfm.mtx)
textstat_frequency(dfm.mtx, n=15)
mostFreqTerms.mtx<-topfeatures(dfm.mtx, n = nfeat(dfm.mtx))
head(mostFreqTerms.mtx, n= 15)
#3.2 recycling
relFreq.MostFreqTerms.mtx<-mostFreqTerms.mtx/sum(mostFreqTerms.mtx)
head(relFreq.MostFreqTerms.mtx)
relPct.MostFreqTerms.mtx<-relFreq.MostFreqTerms.mtx*100
head(relPct.MostFreqTerms.mtx)
pct.dfm.mtx<-dfm_weight(dfm.mtx, scheme = "prop")*100
summary(pct.dfm.mtx)
pct.dfm.mtx[1:10,1:10]

####cosine sim
dist<-textstat_simil(dfm.mtx, margin="documents", method="cosine")
head(dist)
print(dist)
as.data.frame(as.matrix(dist))
