# setwd("C:/Users/lb3_local/OneDrive/sync/git/loubill/Domino/R/MTX")
setwd("F:/OneDrive/sync/git/loubill/Domino/R/MTX")
# setwd("/media/louis/EXCHWINLIN/Gdrive/sync/git/loubill/Domino/R/MTX")
library(tm)
library(quanteda)
library(stringr)

######import données
####test 1 http://docs.quanteda.io/articles/quickstart.html
df.mtx<-read.csv("data/mtx.csv", sep = "\n" ,header=F, quote = "", stringsAsFactors = F, fileEncoding = "UTF8")
write.table(df.mtx, "data/mtx.txt", sep="\t")
vec.mtx<-readtext::readtext("data/mtx.txt", text_field=NULL)
txt.mtx<-NULL
i<-0
for (i in 1 : nrow(df.mtx)){
  txt.mtx<-paste0(df.mtx, df.mtx[i,])
}
# data.mtx<-data.frame(text=df.mtx$V1, stringsAsFactors = FALSE)
# tdm.mtx <- TermDocumentMatrix(Corpus(DataframeSource(df.mtx)))
corpus.1.mtx<-corpus(txt.mtx)
summary(corpus.1.mtx)
dfm.1.mtx<-dfm(corpus.1.mtx, tolower=TRUE, stem = FALSE)
dfm.1.mtx
summary(dfm.1.mtx)

###test 2 http://docs.quanteda.io/articles/quickstart.html
corpus.2.mtx<-corpus(vec.mtx)
summary(corpus.2.mtx)
dfm.2.mtx<-dfm(corpus.2.mtx, tolower=TRUE, stem = FALSE)
dfm.2.mtx
summary(dfm.2.mtx)

####test 3
vec1.mtx<-scan(file = "data/mtx1.csv", sep="\t", what = character(), quote = "")
vec2.mtx<-scan(file = "data/mtx2.csv", sep="\t", what = character(), quote = "")
vec3.mtx<-scan(file = "data/mtx3.csv", sep="\t", what = character(), quote = "")
vec.mtx<-c(vec1.mtx, vec2.mtx, vec3.mtx)
dfm.mtx<-dfm(vec.mtx)
topfeatures(dfm.mtx)
dfm.mtx
summary(dfm.mtx)

###construction modèle LSA
mylsa<-textmodel_lsa(dfm.mtx)
mylsa$docs
mylsa$features
mylsa$matrix_low_rank

####https://docs.quanteda.io/articles/pkgdown/examples/chinese.html
corp.expl.mtx<-corpus(vec.mtx)
tok.corp.mtx<-tokens(corp.expl.chin)
fcm.mtx<-fcm(tok.corp.mtx, context = "window")
fcm.mtx

####https://docs.quanteda.io/articles/pkgdown/examples/twitter.html
dfm.mtx.expl1<-dfm(vec.mtx)
dfm.mtx.expl1
vec4.mtx<-scan(file = "data/mtx.csv", sep="\t", what = character(), quote = "")
dfm.mtx.expl2<-dfm(vec4.mtx)
dfm.mtx.expl2
fcm(dfm.mtx.expl2)
head(fcm(dfm.mtx.expl2))
