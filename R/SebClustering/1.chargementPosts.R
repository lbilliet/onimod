rm(list = )
####### Le format CSV des post est récupéré par requete SPARQL
### charger l'un de ces fichiers :

# df <- read.table("data/IVG.csv",sep=";",header=T,comment.char = "", quote="", stringsAsFactors = F,fileEncoding="utf8")
# df <- read.table("DEPRESSION.txt",sep="\t",header=F, comment.char = "", quote="", stringsAsFactors = F, fileEncoding="utf8")
df <- read.table("data/Contraception.csv",sep="\t",header=T, comment.char = "", quote="", stringsAsFactors = F, fileEncoding="UTF8")
# pourquoi avertissements ?
colnames(df) <- gsub("^X.","",colnames(df))
bool <- df$contentPost == ""
sum(bool) ## posts vides
df <- subset (df, !bool)


# ### nombre de caractères :
# ncar <- nchar(df$contentPost)
# ncar
# table(ncar)#ici la table créée rapporte les nb de char avec la fréquence des posts avec cette fréquence
# ## je supprime les posts qui n'ont quasiment pas de contenu :
# bool <- ncar < 72
# sum(bool)
# print(subset (df,bool))
# df <- subset (df, !bool)

library(koRpus)
##################### Etape1 lemmatisation
########### appeler treetagger en ligne de commande :

## chemin vers TreeTagger pour lemattiser
cheminTT <- "C:/Users/lb3/OneDrive/not_sync/Domino/TreeTagger"
## ecrire un fichier :
separateur <- "\nAAAAAZZZZZ\n"
df$contentPost <- tolower(df$contentPost)
exemples <- paste(df$contentPost,collapse=separateur)
writeLines(exemples, "exemples.txt")
system.time(
  tagged.text <- treetag("exemples.txt", treetagger="manual",lang="fr",
                         TT.options=list(path=cheminTT, preset="fr", abbrev="french-abbreviations",
                                         tknz.opts="-f")
                         , debug=F)
)
# Error: Specified file cannot be found:
# C:/Users/lb3/OneDrive/not_sync/Domino/tree-tagger-windows-3.2/TreeTagger/lib/french-utf8.par
# In addition: Warning message:
#   UTF-8 is now the default encoding, please rename your preset from "fr-utf8" into just "fr"!
## 2 013 posts en 4,3 minutes pour IVG
### 9 962 posts en 29 minutes pour Contraception

voir <- tagged.text@TT.res

### ces résultats sont sauvegardés :
# ivg.rdata pour les posts d'IVG
# contraception.rdata pour les posts contraception
## passer maintenant aux méthodes de clustering

############test tags alternatifs
write.table(df$contentPost,
            file = "data/contentPostContraception.txt",
            sep = "\t",
            row.names = F,
            col.names = F,
            fileEncoding = "UTF-8")#export contenu df$contepost en txt
tagged.text.obj<-tokenize("data/contentPostContraception.txt",
                          lang="fr",
                          detect=c(parag=T, hline=T))#tokenization via outil koRpus
taggedText(tagged.text.obj)#visualisation résultat
language(tagged.text.obj)
summary(tagged.text.obj)
plot(tagged.text.obj)

tagged.text.objS4 <- treetag("data/contentPostContraception.txt", 
            treetagger="manual",
            lang="fr", 
            TT.options= list (path="C:/Users/lb3/OneDrive/not_sync/Domino/TreeTagger", preset="fr"))
language(tagged.text.objS4)
