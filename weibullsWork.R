#weibullswork
#March 1 2013:  moving all my one-off examples from weibulls.R so I can source weibulls.R

library("fitdistrplus")
library("plyr")
setwd("C:/Users/tbaer/Desktop/cbm/october redo weibulls/R work")
weibulls_initial<-read.csv("newTOW_LOC.csv",header=TRUE)
#weibulls_initial<-read.csv("./newTOW_LOC_10.01.2010.NoOptions.csv",header=TRUE)
weibulls_initial<-read.csv("./newTOW_LOC_diffOrder.csv",header=TRUE)

source("weibulls.R")
weibullPN_info<-read.csv("tblWUCinfo.csv",header=TRUE)


system.time(allweibulls<- gatherallweibulls(weibulls_initial,
                                            plot=FALSE,modkm=TRUE,plotdir="./plots/")) #ALL THE WEIBULLz
write.csv(allweibulls,file="Apache Weibulls.csv",quote=FALSE)

subdf<-weibulls_initial[weibulls_initial$WUC=="06A" & weibulls_initial$PN=="7-311310001-43" ,]
subdf<-droplevels(subdf)
censsubdf<-CensUncens(subdf)
weibsubdf<-fitdistcens(censsubdf,"weibull")

ranksubdf<-getModKM(censsubdf,weibsubdf[[1]])
#getPlotFromDF(ranksubdf,subdf,weibsubdf[[1]],"WUCadjRepInt")


getwd()
setwd("./plots")
setwd("..")
getwd()

source("stattest.R")
system.time(alltests<-testallweibulls(allweibulls,weibulls_initial,doAllGroups=FALSE))
write.csv(alltests,file="Apache Tests.csv",quote=FALSE)

b<-0
for(ii in 2:(11-1)) {
  b<-b+dim(combn(11,ii))[2]
}

# Write up the markdown file and then convert it to HTML
require(knitr)
require(markdown)
knit("C:\\Users\\tbaer\\Desktop\\cbm\\october redo weibulls\\R work\\Rdocumentation.Rmd")
knit2html("C:\\Users\\tbaer\\Desktop\\cbm\\october redo weibulls\\R work\\Rdocumentation.Rmd")
knit2pdf("C:\\Users\\tbaer\\Desktop\\cbm\\october redo weibulls\\R work\\Rdocumentation.Rmd")

spin("C:\\Users\\tbaer\\Desktop\\cbm\\october redo weibulls\\R work\\Rdocumentation.Rmd",format="Rnw")
