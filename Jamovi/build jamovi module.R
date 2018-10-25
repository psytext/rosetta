
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

## The downloaded source packages are in
##  ‘/private/var/folders/c6/1sl9f3jd0rq9rj1dryclrspw0000gn/T/RtmpWCd3z8/downloaded_packages’


jmvtools::check()
jmvtools::check(home='C:\\Program Files\\jamovi 0.9.2.8\\bin')

## jamovi found at /Applications/jamovi.app


jmvtools::install()        # adds a module to jamovi

# install.packages("jmvcore")
# library(jmvcore)

# setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/R stuff/Mediatie_R")
# jmvtools::create('SuperAwesome')
# setwd('SuperAwesome')
# jmvtools::addAnalysis(name='ttest', title='Independent Samples T-Test')
# jmvtools::install()

# devtools::install()
# library(SuperAwesome)
# data(ToothGrowth)
# ttest(data=ToothGrowth, dep='len', group='supp')

# ************************

setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/Jamovi")
setwd("D:/R Git projects/rosetta")

jmvtools::create('semMed')

setwd('semMed')
jmvtools::addAnalysis(name='modmed', title='Moderated mediation')
jmvtools::install()
