
library("devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/Jamovi")

devtools::create("semMed")

setwd("./semMed")

devtools::document()

#devtools::build_vignettes(pkg= "cyclic")
devtools::use_data(dat1, semMed)

devtools::use_package("lavaan")
devtools::use_package("ggplot2") 
devtools::check(document = TRUE)

setwd("..")
devtools::install("semMed")


library(semMed)
