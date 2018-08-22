setwd("C:/Users/lb3/OneDrive/sync/git/loubill/Domino/R/Domino/MTX")
#install.packages("devtools")
# devtools::install_github("lbilliet/loubill")
# slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
# install_url(slam_url)
# urlSlam <- "https://cran.r-project.org/bin/windows/contrib/3.6/slam_0.1-43.zip"
# install_url(urlSlam)
# install.packages('installr')
# installr::install.Rtools()
# install_github("bmschmidt/wordVectors")
#devtools::install_github("bmschmidt/wordVectors")
#install.packages(c('tm', 'SnowballC', 'wordcloud', 'topicmodels'))
library(devtools)
library(tm)
library(SnowballC)
library(wordcloud)
library(topicmodels)
library("koRpus")
library("wordVectors")
dfMtx<-read.csv2("export.csv", header=F, stringsAsFactors = F)

#boucle matrice avec post vides selon colonnes
bool=data.frame(matrix(ncol=ncol(dfMtx),nrow =nrow(dfMtx)))
for (i in 1:ncol(dfMtx)){
  bool[,i]<-dfMtx[,i]==""
}
sum(bool) #nb posts vides
dfMtx<-subset (dfMtx, !bool)#suppr posts vides
##### PROBLEME ?

# summary(dfMtx)
# result <- NULL
# dfMtx$contanetate <- ''
# for (i in 1 : nrow(dfMtx)){
# 
#   for (j in 1 : ncol(dfMtx)){
#     if (j == 1){
#       result <- dfMtx[i, j]
#     } else {
#       result <- paste0(result, dfMtx[i, j])
#     }
#   }
#   dfMtx$contanetate[i] <- result
# }
# dfMtx[35,5]

#constitution du corpus ? partir des donn?es collig?es de la colonne Summary du DF reviews
postsMtxCorpus = Corpus(VectorSource(dfMtx))
postsMtxCorpus

#?tapes de normalization des termes du corpus avec :
# 1 tout en minuscules
postsMtxCorpus = tm_map(postsMtxCorpus, content_transformer(tolower))
# 2 suppresion des nombres
postsMtxCorpus = tm_map(postsMtxCorpus, removeNumbers)
# 3 suppression ponctuation
postsMtxCorpus = tm_map(postsMtxCorpus, removePunctuation)
#constitution liste stop words
stopWordsFrA <- c("sioc","a","abord","absolument","afin","ah","ai","aie","aient","aies","ailleurs","ainsi","ait","allaient","allo","allons","allô","alors","anterieur","anterieure","anterieures","apres","après","as","assez","attendu","au","aucun","aucune","aucuns","aujourd","aujourd'hui","aupres","auquel","aura","aurai","auraient","aurais","aurait","auras","aurez","auriez","aurions","aurons","auront","aussi","autre","autrefois","autrement","autres","autrui","aux","auxquelles","auxquels","avaient","avais","avait","avant","avec","avez","aviez","avions","avoir","avons","ayant","ayez","ayons","b","bah","bas","basee","bat","beau","beaucoup","bien","bigre","bon","boum","bravo","brrr","c","car","ce","ceci","cela","celle","celle-ci","celle-là","celles","celles-ci","celles-là","celui","celui-ci","celui-là","celà","cent","cependant","certain","certaine","certaines","certains","certes","ces","cet","cette","ceux","ceux-ci","ceux-là","chacun","chacune","chaque","cher","chers","chez","chiche","chut","chère","chères","ci","cinq","cinquantaine","cinquante","cinquantième","cinquième","clac","clic","combien","comme","comment","comparable","comparables","compris","concernant","contre","couic","crac","d","da","dans","de","debout","dedans","dehors","deja","delà","depuis","dernier","derniere","derriere","derrière","des","desormais","desquelles","desquels","dessous","dessus","deux","deuxième","deuxièmement","devant","devers","devra","devrait","different","differentes","differents","différent","différente","différentes","différents","dire","directe","directement","dit","dite","dits","divers","diverse","diverses","dix","dix-huit","dix-neuf","dix-sept","dixième","doit","doivent","donc","dont","dos","douze","douzième","dring","droite","du","duquel","durant","dès","début","désormais","e","effet","egale","egalement","egales","eh","elle","elle-même","elles","elles-mêmes","en","encore","enfin","entre","envers","environ","es","essai","est","et","etant","etc","etre","eu","eue","eues","euh","eurent","eus","eusse","eussent","eusses","eussiez","eussions","eut","eux","eux-mêmes","exactement","excepté","extenso","exterieur","eûmes","eût","eûtes","f","fais","faisaient","faisant","fait","faites","façon","feront","fi","flac","floc","fois","font","force","furent","fus","fusse","fussent","fusses","fussiez","fussions","fut","fûmes","fût","fûtes","g","gens","h","ha","haut","hein","hem","hep","hi","ho","holà","hop","hormis","hors","hou","houp","hue","hui","les","même","mon","comme")
stopWordsFrB<-c("huit","huitième","hum","hurrah","hé","hélas","i","ici","il","ils","importe","j","je","jusqu","jusque","juste","k","l","la","laisser","laquelle","las","le","lequel","les","lesquelles","lesquels","leur","leurs","longtemps","lors","lorsque","lui","lui-meme","lui-même","là","lès","m","ma","maint","maintenant","mais","malgre","malgré","maximale","me","meme","memes","merci","mes","mien","mienne","miennes","miens","mille","mince","mine","minimale","moi","moi-meme","moi-même","moindres","moins","mon","mot","moyennant","multiple","multiples","même","mêmes","n","na","naturel","naturelle","naturelles","ne","neanmoins","necessaire","necessairement","neuf","neuvième","ni","nombreuses","nombreux","nommés","non","nos","notamment","notre","nous","nous-mêmes","nouveau","nouveaux","nul","néanmoins","nôtre","nôtres","o","oh","ohé","ollé","olé","on","ont","onze","onzième","ore","ou","ouf","ouias","oust","ouste","outre","ouvert","ouverte","ouverts","o|","où","p","paf","pan","par","parce","parfois","parle","parlent","parler","parmi","parole","parseme","partant","particulier","particulière","particulièrement","pas","passé","pendant","pense","permet","personne","personnes","peu","peut","peuvent","peux","pff","pfft","pfut","pif","pire","pièce","plein","plouf","plupart","plus","plusieurs","plutôt","possessif","possessifs","possible","possibles","pouah","pour","pourquoi","pourrais","pourrait","pouvait","prealable","precisement","premier","première","premièrement","pres","probable","probante","procedant","proche","près","psitt","pu","puis","puisque","pur","pure","q","qu","quand","quant","quant-à-soi","quanta","quarante","quatorze","quatre","quatre-vingt","quatrième","quatrièmement","que","quel","quelconque","quelle","quelles","quelqu'un","quelque","quelques","quels","qui","quiconque","quinze","quoi","quoique","r","rare","rarement","rares","relative","relativement","remarquable","rend","rendre","restant","reste","restent","restrictif")
stopWordsFrC<-c("retour","revoici","revoilà","rien","s","sa","sacrebleu","sait","sans","sapristi","sauf","se","sein","seize","selon","semblable","semblaient","semble","semblent","sent","sept","septième","sera","serai","seraient","serais","serait","seras","serez","seriez","serions","serons","seront","ses","seul","seule","seulement","si","sien","sienne","siennes","siens","sinon","six","sixième","soi","soi-même","soient","sois","soit","soixante","sommes","son","sont","sous","souvent","soyez","soyons","specifique","specifiques","speculatif","stop","strictement","subtiles","suffisant","suffisante","suffit","suis","suit","suivant","suivante","suivantes","suivants","suivre","sujet","superpose","sur","surtout","t","ta","tac","tandis","tant","tardive","te","tel","telle","tellement","telles","tels","tenant","tend","tenir","tente","tes","tic","tien","tienne","tiennes","tiens","toc","toi","toi-même","ton","touchant","toujours","tous","tout","toute","toutefois","toutes","treize","trente","tres","trois","troisième","troisièmement","trop","très","tsoin","tsouin","tu","té","u","un","une","unes","uniformement","unique","uniques","uns","v","va","vais","valeur","vas","vers","via","vif","vifs","vingt","vivat","vive","vives","vlan","voici","voie","voient","voilà","vont","vos","votre","vous","vous-mêmes","vu","vé","vôtre","vôtres","w","x","y","z","zut","à","â","ça","ès","étaient","étais","était","étant","état","étiez","étions","été","étée","étées","étés","êtes","être","ô","uri","binding","undefined","xml","xmlns","head","results","result","sparql","http","name","literal","http://forum.doctissimo.fr/medicaments/antidepresseurs-anxiolytiques/","http://forum.doctissimo.fr/medicaments/","http://forum.doctissimo.fr/","http://forum.doctissimo.fr/sante/Endometriose/","http://forum.doctissimo.fr/sante/","http://forum.doctissimo.fr/grossesse-bebe/grossesse-extra-uterine/","http://forum.doctissimo.fr/grossesse-bebe/","http://forum.doctissimo.fr/grossesse-bebe/Tests-symptomes-grossesse/")
stopWordsFr<-c(stopWordsFrA, stopWordsFrB, stopWordsFrC)

# 4 suppression stopwords
postsMtxCorpus = tm_map(postsMtxCorpus, removeWords, stopWordsFr)
# 5 suppression espaces en extra
postsMtxCorpus =  tm_map(postsMtxCorpus, stripWhitespace)
inspect(postsMtxCorpus[1:10])
#constitution matrice fr?q de termes
postsMtxDtm<- DocumentTermMatrix(postsMtxCorpus)
postsMtxDtm
inspect(postsMtxDtm[500:505, 500:505])

#exclusion des termes les moins fr?quents pour que sparsity < 0.95
postsMtxDtm = removeSparseTerms(postsMtxDtm, 0.975)
postsMtxDtm
inspect(postsMtxDtm[1:20,1:20])

#simple word cloud
findFreqTerms(postsMtxDtm)

freq = data.frame(sort(colSums(as.matrix(postsMtxDtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(4, "Dark2"))
