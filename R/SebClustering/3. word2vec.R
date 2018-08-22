
library(wordVectors)
rm(list=ls())
###################### Word2Vec Contraception ##############################
#### créer le corpus avec 2.LDA.R puis : 
### exporter le corpus au format txt pour word2vec : 
# texte <- unlist(sapply(myCorpusContraception, `[`, "content"))
# length(texte)
# writeLines(texte,"Contraception.txt")

### paramétrisation du modèle :
model = train_word2vec("data/Contraception.txt",output="data/Contraception.bin",threads = 3,vectors = 100,window=12,
                       force=T)
## chargement du modèle 
model = read.vectors("data/Contraception.bin")
names(nearest_to(model,model[[c("cerazette")]]))
nearest_to(model,model[[c("cerazette","microval","desopop","luteran","desogestrel",
                          "belara","optimizette","melodia","moneva","zoely","efezial",
                          "cycleane","gestodene")]],50)


######################## Word2Vec  IVG ###############################################"
model = train_word2vec("data/ivg.txt",output="data/ivg.bin",threads = 3,vectors = 100,window=12,
                       force=T)
model = read.vectors("data/ivg.bin")
nearest_to(model,model[["pilule"]],10)
nearest_to(model,model[["vouloir"]],10)
nearest_to(model,model[["enfant"]],10)
nearest_to(model,model[["ivg"]],10)
nearest_to(model,model[["triste"]],10)

correctionOrthographe <- function(model, medicament){
  library(stringdist)
  proches <- names(nearest_to(model,model[[medicament]],100))
  stringdist(medicament, proches,method = "soundex")
  distances <- stringdist(medicament, proches,method = "lv")
  temp <- data.frame(terme=proches, distance = as.numeric(distances))
  temp$soundex <- stringdist(medicament, proches,method = "soundex")
  temp2 <- subset (temp, distance < 3 | soundex==0)
  return(as.character(temp2$terme))
}
correctionOrthographe(model, medicament)
correctionOrthographe(model, "pillule")

######Trouver des synonymes dans le corpus de dépression  #########
### quelques tests :
model = read.vectors("data/depression.bin")
names(nearest_to(model,model[["deprime"]],10))
resultat <- names(nearest_to(model,model[["poids"]],20))
names(nearest_to(model,model[[resultat]],20))
nearest_to(model,model[["paroxetine"]],10)
names(nearest_to(model,model[["angoisse"]],10))
nearest_to(model,model[["dormir"]],10)
nearest_to(model,model[["nuit"]],10)
nearest_to(model,model[["angoisse"]],10)
nearest_to(model,model[["bien"]],10)
project(model,model[["angoisse"]])
nearest_to(model,model[["antidepresseur"]])
nearest_to(model,model[["deroxat"]] - model[["antidepresseur"]] + model[["neuroleptique"]])
nearest_to(model,model[["deroxat"]] - model[["depression"]] + model[["angoisse"]])
nearest_to(model,model[["deroxat"]] - model[["depression"]] + model[["poids"]])

nearest_to(model,model[[c("deroxat","paroxetine","seroplex")]],50)

nearest_to(model,model[["poids"]],10)
nearest_to(model,model[["medicament"]])
medicament <- "seroplex"
medicament <- "effexor"
medicament <- "propranolol"
medicament <- "cerazette"
correctionOrthographe(model, medicament)


