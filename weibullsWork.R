# Create some Weibull Output 
# March 1 2013:  moving all my one-off examples from weibulls.R so I can source weibulls.R
# January 2015: updated to use in the new clockworkETL

library("fitdistrplus")
library("plyr")
library("dplyr")
library("tidyr")

# get script path to get correct relative path for files
args <- commandArgs(trailingOnly = FALSE)
scriptPath <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
if (interactive()) scriptPath <- getwd()
setwd(scriptPath)

#setwd("C:/Users/tbaer/Desktop/cbm/october redo weibulls/R work")
#weibulls_initial<-read.csv("newTOW_LOC.csv",header=TRUE)
weibulls_initial<-read.csv("./newTOW_LOC_small.csv",header=TRUE)

source("./weibull_functions.R")
#weibullPN_info<-read.csv("./tblWUCinfo.csv",header=TRUE)
wucnames<-read.csv("./wucnames.csv",header=TRUE)

reliability_parameter <- read.csv("./reliability_parameter.csv", stringsAsFactors = FALSE)
reliability_interval <- read.csv("./reliability_interval_small.csv", stringsAsFactors = FALSE)
reliability_interval_parameter <- read.csv("./reliability_interval_parameter_small.csv", stringsAsFactors = FALSE)
weibulls_initial <- mergeWeibInput(reliability_parameter,reliability_interval,reliability_interval_parameter)
# calculate all weibulls
system.time(allweibulls <- gatherallweibulls(weibulls_initial,reliability_parameter,verbose=TRUE,unbug=FALSE,
                                            plot=FALSE,modkm=TRUE,plotdir="./plots/")) #ALL THE WEIBULL
write.csv(allweibulls,file="Apache Weibulls.csv",quote=FALSE)
# perform distribution comparison tests
source("./stattest_functions.R")
system.time(alltests <- testallweibulls(allweibulls,weibulls_initial,reliability_parameter,
                                      verbose=TRUE,doAllGroups=TRUE))
write.csv(alltests,file="Apache Tests.csv",quote=FALSE)

