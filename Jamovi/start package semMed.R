
library("devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/Jamovi")
setwd("D:/R Git projects/rosetta")

devtools::create("semMed")

setwd("./semMed")

devtools::document()

#devtools::build_vignettes(pkg= "cyclic")
devtools::use_data(dat1, overwrite = TRUE)

devtools::use_package("lavaan")
devtools::use_package("ggplot2")
devtools::use_package("dplyr")
devtools::use_package("stringr")
devtools::use_package("pander")
devtools::check(document = TRUE)

setwd("..")
devtools::install("semMed")


library(semMed)
