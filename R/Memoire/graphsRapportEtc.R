rm(list=ls())
setwd(dirname(file.choose()))
getwd()
library(xtable)
library(stringr)
library(ggplot2)
library(dplyr)

df_longueur<-read.csv(file = "contentLength.csv", header = T, sep = "\t")
df_date<-read.csv(file = "dateCreated.csv", header = T, sep = "\t")
sum(df_date$nPosts)
sum(df_longueur$nPosts)

df_date$dateCreated<-as.Date(df_date$dateCreated, "%Y-%m-%d")
df_date <- df_date[order(df_date$dateCreated),]
df_date
plot(df_date$nPosts~df_date$dateCreated,type="b", xlab="label des x",ylab="label des y", main="titre")

summary(df_longueur)
summary(df_date)


voir = getDiseasesCount(host,port,index,type)
sum(voir$frequency)

# seqDays <- seq(min(df_date$dateCreated), max(df_date$dateCreated),by="days")
# seqDays <- data.frame(dateCreated = seqDays)
# seqDays$n <- 0
# df_date2 <- merge (df_date, seqDays, by="dateCreated",all.x=T)


# ggplot(df_longueur, aes(x=nPosts, y=df_longueur$nchar), geom_jitter(position = position_jitter(10)), coord_flip())
# ggplot (df_longueur, aes(sample = nPosts), stat_qq())
# ggplot(df_longueur, aes(x=df_longueur$nPosts, y=df_longueur$nchar), geom_histogram())
# pairs(nchar~nPosts, df_longueur)

plot_nchar <-
   smoothScatter(
      df_longueur$nchar,
      df_longueur$nPosts,
      nrpoints = 2000,
      colramp = colorRampPalette(c("white", blues9, "red")),
      xlab = "Nombre de caractères",
      # main = "Effectifs des posts selon le nombre de caractères",
      ylab = "Nombre de posts"
   )
plot_log_nchar <-
   smoothScatter(
      df_longueur$nchar,
      log(df_longueur$nPosts),
      xlim = c(0, 7000),
      nrpoints = 4000,
      colramp = colorRampPalette(c("white", blues9, "red")),
      xlab = "Nombre de caractères (troncature < 10 000)",
      # main = "Effectifs des posts (log) selon le nombre de caractères (trunc)",
      ylab = "Nombre de posts (log)"
   )


summary(df_date$dateCreated)
df_date$dateCreated = as.POSIXct(strptime(df_date$dateCreated, format="%Y-%m-%d"))
plot(as.Date(df_date$dateCreated),log(df_date$nPosts))
axis.Date(3,Month,at=seq(as.Date("2010/01/01"), as.Date("2017/01/30"),by="month"))

plot_date_base<-plot(df_date$dateCreated, log(df_date$nPosts), xaxt = "n", type = "p")
axis(1, df_date$dateCreated, format(df_date$dateCreated, "%b %y"), cex.axis = 0.6)
ggplot( data = df_date, aes( dateCreated, nPosts )) + geom_line()

list_date = as.Date(df_date$dateCreated)
cut(list_date, breaks = "month")

# df_date$dateCreated<-as.POSIXlt(df_date$dateCreated)
# mon<-df_date$dateCreated$mon
# yr<-df_date$dateCreated$year
# monyr<-as.factor(paste(mon,yr,sep="/"))
# df_date<-monyr


df_date<-read.csv(file = "dateCreated.csv", header = T, sep = "\t")
plot_date <-
  smoothScatter(
    df_date$dateCreated,
    df_date$nPosts,
    nrpoints = 2000,
    colramp = colorRampPalette(c("white", blues9, "red")),
    xlab = "Date de création",
    ylab = "Nombre de posts",
    #main = "Effectifs des posts selon leur date de création",
    xaxt = "n"
  )
axis(  1,  at = df_date$dateCreated,  labels = lapply(df_date$dateCreated, function(d)    strftime(d, "%m-%Y")),  tick = FALSE)
plot_date2 <-
  smoothScatter(
    df_date$dateCreated,
    log(df_date$nPosts),
    nrpoints = 2000,
    colramp = colorRampPalette(c("white", blues9, "red")),
    xlab = "Date de création",
    ylab = "Nombre de posts (log)",
    # main = "Effectifs des posts (log) selon leur date de création",
    xaxt = "n"
  )
axis(  1,  at = df_date$dateCreated,  labels = lapply(df_date$dateCreated, function(d)    strftime(d, "%m-%Y")),  tick = FALSE)
# axis(  1,  at = df_date$dateCreated,  labels = lapply(df_date$dateCreated, function(d)    strftime(d, "%F")),  tick = FALSE)
days <-  seq(min(df_date$dateCreated), max(df_date$dateCreated), by = 'month')
days <-  seq(as.Date(min(df_date$dateCreated)), as.Date(max(df_date$dateCreated)), by = 'month')
axis( 1,  at = days, labels=strftime(days, "%F"),    strftime(days, "%F"),  tick = FALSE)

# plot_log_date<-smoothScatter(df_date$dateCreated, log(df_date$nPosts), xlim = c(0, 7000), nrpoints = 4000, colramp = colorRampPalette(c("white", blues9, "red")), xlab = "Nombre de caractères (troncature < 10 000)", ylab = "Nombre de posts (log)", main = "Effectifs des posts (log) selon le nombre de caractères (trunc)")


length(df_longueur[,1])
nb_char<-vector('numeric', length = sum(df_longueur$nPosts))
j<-0
for (i in 1 : length(df_longueur[,1])){
  
  for (j in length(nb_char) : df_longueur$nPosts[i]){
    nb_char[j]<-df_longueur$nchar[i]
  }
  
}

# df_longueur[(length(df_longueur)+i)]<- (df_longueur[1,i]*df_longueur[2,i])

