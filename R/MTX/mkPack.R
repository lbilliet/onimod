# setwd("C:/Users/lb3/OneDrive/sync/git/loubill/Domino/R/Domino/MTX")
setwd("/media/louis/EXCHWINLIN/Gdrive/sync/git/loubill/Domino/R/Domino/MTX/dominalgo/")
library(devtools)
install.packages("roxygen2")
library(roxygen2)
install.packages(mclustcomp)
library("mclustcomp")
create("dominalgo")
package.skeleton(name = "dominalgo", list, environment = .GlobalEnv, path = ".", force = FALSE, code_files = )