rm(list = ls())
setwd(dirname(file.choose()))
library(udpipe)
library(xtable)
library(stringr)
library(dplyr)
library(ggplot2)

#DL modèles pour tests
udmodel <- udpipe_download_model(language = "french")
View(udmodel)
str(udmodel)
udmodelDL <- udpipe_load_model(file = udmodel$file_model)
View(udmodelDL)
str(udmodelDL)
udmodelDL$file
str(udmodelDL$file)
udmodelDL$model
str(udmodelDL$model)

#test d'anotation avec phrase perso
x <-
   udpipe_annotate(udmodelDL, x = "Je suis en train de tester le package udpipe sur mon ordinateur : si cela fonctionne je serai de bonne humeur. Je teste ma suite de fonction pour la répétition.")
str(x)
View(x)
#conversion de l'annotation en une DF lisible / exploitable
x <- as.data.frame(x, detailed = TRUE)
str(x)
View(x)


#test conversion encodage
x <-
   udpipe_annotate(
      udmodelDL,
      x = iconv(
         "Je suis en train de tester le package udpipe sur mon ordinateur : si cela fonctionne je serai de bonne humeur. Je teste ma suite de fonction pour la répétition.",
         to = 'UTF-8'
      )
   )
str(x)
x <- as.data.frame(x, detailed = TRUE)
str(x)
View(x)
table(x$upos)

dfTable <- data.frame(
   String = character(),
   Date = as.Date(character()),
   File = character(),
   User = character(),
   stringsAsFactors = FALSE
)
ncol(x)
for (i in 1:ncol(x)) {
   dfTable[i] <- table(x[, i])
}

### test intro https://bnosac.github.io/udpipe/docs/doc1.html
vignette("udpipe-tryitout", package = "udpipe")
vignette("udpipe-annotation", package = "udpipe")
vignette("udpipe-usecase-postagging-lemmatisation", package = "udpipe")
# An overview of keyword extraction techniques: https://bnosac.github.io/udpipe/docs/doc7.html
vignette("udpipe-usecase-topicmodelling", package = "udpipe")
vignette("udpipe-train", package = "udpipe")