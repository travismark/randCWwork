# Create some Weibull Output 
# March 1 2013:  moving all my one-off examples from weibulls.R so I can source weibulls.R
# January 2015: updated to use in the new clockworkETL

library("fitdistrplus")
library("plyr")

# get script path to get correct relative path for files
args <- commandArgs(trailingOnly = FALSE)
scriptPath <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
if (interactive()) scriptPath <- getwd()
setwd(scriptPath)

#setwd("C:/Users/tbaer/Desktop/cbm/october redo weibulls/R work")
#weibulls_initial<-read.csv("newTOW_LOC.csv",header=TRUE)
weibulls_initial<-read.csv("./newTOW_LOC.csv"),header=TRUE)

source("./weibulls.R")
weibullPN_info<-read.csv("./tblWUCinfo.csv",header=TRUE)

system.time(allweibulls<- gatherallweibulls(weibulls_initial,
                                            plot=FALSE,modkm=TRUE,plotdir="./plots/")) #ALL THE WEIBULL
write.csv(allweibulls,file="Apache Weibulls.csv",quote=FALSE)

source("./stattest.R")
system.time(alltests<-testallweibulls(allweibulls,weibulls_initial,doAllGroups=FALSE))
write.csv(alltests,file="Apache Tests.csv",quote=FALSE)