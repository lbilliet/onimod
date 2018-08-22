rm(list=ls())
stringFile <- "Contraception.csv"

df <- read.table(stringFile,sep="\t",header=T, comment.char = "", quote="", stringsAsFactors = F)
colnames(df) <- gsub("^X.","",colnames(df))
courbes <- function(df, stringFile){
  library(stringr)
  ## extraction de la date
  dates <- str_extract(df$datePost,"[0-9]{4}-[0-9]{2}-[0-9]{2}")
  tab <- table(dates)
  dates <- data.frame(dates=names(tab), N = as.numeric(tab))
  dates$dates <- as.Date(dates$dates, format="%Y-%m-%d")
  cat ("date minimale : ")
  print(min(dates$dates))
  cat ("date maximale : ")
  print((max(dates$dates)))
  dates$jour <- weekdays(dates$dates)
  dates <- dates[order(dates$dates),]
  ### ajout une fréquence de 0 lorsque la date n'existe pas (pas de post ce jour) : 
  alldates <- seq(min(dates$dates), max(dates$dates),by="day")
  alldates <- data.frame(dates=alldates)
  dates2 <- merge (dates, alldates, by="dates", all.y=T)
  bool <- is.na(dates2$N)
  dates2$N[bool] <- 0
  dates$jour <- weekdays(dates$dates)
  dates <- dates[order(dates$dates),]
  
  #### jours de la semaine
  plot (dates$N, type="l", xaxt="n", xlab="dates",ylab="Nombre de posts par jour", main=stringFile)
  sequence <- seq(from=1, to=nrow(dates), by=nrow(dates)/5)
  sequence <- round(sequence)
  sequence <- append(sequence, nrow(dates))
  axis(side=1, at=sequence, labels=as.character(dates$dates[sequence]))
  
  #### lisser 
  slideFunct <- function(data, window, step){
    total <- length(data)
    spots <- seq(from=1, to=(total-window), by=step)
    result <- vector(length = length(spots))
    i <- 1
    for(i in 1:length(spots)){
      result[i] <- mean(data[spots[i]:(spots[i]+window-1)])
    }
    return(result)
  }
  
  # ## sur 30 jours : creux du mois d'aout
  lissage <- slideFunct(dates$N, 30, 1)
  plot(lissage,type="l", xaxt="n",ylab="Moyenne du nombre de posts en mois glissant", xlab="",
       main=stringFile)
  sequence <- seq(from=1, to=nrow(dates), by=(nrow(dates)-30)/5)
  sequence <- round(sequence)
  sequence <- append(sequence, nrow(dates))
  axis(side=1, at=sequence, labels=as.character(dates$dates[sequence]))
}

courbes("IVG.csv")
courbes("Contraception.csv")

bool <- grepl("cerazette",df$contentPost)
sum(bool)
df2 <- subset (df, bool)

courbes(df2, "cerazette")

medicaments <- c("cerazette","microval","desopop","luteran","desogestrel",
"belara","optimizette","melodia","moneva","zoely","efezial",
"cycleane","gestodene")
medicaments <- paste(medicaments, collapse="|")
bool <- grepl(medicaments,df$contentPost)
sum(bool)
df2 <- subset (df, bool)

courbes(df2, "contraceptifs")
i <- "cerazette"
for (i in medicaments){
  print(correctionOrthographe(model, i))
}

# ### par mois-année
# dates$annee <- format(dates$dates, format="%m/%Y")
# moisannee <- unique(dates$annee)
# id <- 1:length(moisannee)
# correspondance <- data.frame(mois = moisannee, id = id)
# tab <- tapply(dates$N, dates$annee,sum)
# tab <- data.frame(mois=names(tab), frequence=as.numeric(tab))
# tab2 <- merge (tab, correspondance, by="mois")
# tab2 <- tab2[order(tab2$id),]
# plot(tab2$frequence, xaxt="n", xlab="mois",ylab="Nombre de délivrances", type="b")
# axis(side=1, at=1:nrow(tab2), labels=as.character(tab2$mois))
# 
# ## par mois
# dates$mois <- months(dates$dates)
# dates$mois <- as.factor(dates$mois)
# levels(dates$mois) <- c("8","4","12","2","1","7","6","5","3","11","10","9")
# dates$mois <- as.numeric(as.character(dates$mois))
# tab <- tapply(dates$N, dates$mois, sum)
# # pic de ventes en octobre
# plot(tab, type="o", ylab="nombre de posts",xlab="numéro du mois")
