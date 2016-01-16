# Create some Weibull Output 
# March 1 2013:  moving all my one-off examples from weibulls.R so I can source weibulls.R
# January 2015: updated to use in the new clockworkETL

library("fitdistrplus")
library("plyr")
library("dplyr")
library("tidyr")
library("RODBC")

# get script path to get correct relative path for files
args <- commandArgs(trailingOnly = FALSE)
scriptPath <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
if (interactive()) scriptPath <- getwd()
setwd(scriptPath)

user.name <- "root"
pw <- "password"
server <- "localhost"
port <- 3306
db <- "etl_workspace_dev"
conn <- odbcDriverConnect(paste0("DRIVER={MySQL ODBC 5.3 ANSI Driver};Server=",server,";Port",port,";Database=",db,";UID=",user.name,";PWD=",pw))


source("./weibull_functions.R")
#weibullPN_info<-read.csv("./tblWUCinfo.csv",header=TRUE)
class_names <- read.csv("./class_names.csv",header=TRUE)

reliability_parameter <- sqlQuery(conn, "SELECT * FROM reliability_parameter")
reliability_interval <- sqlQuery(conn, "SELECT * FROM reliability_interval")
reliability_interval_parameter <- sqlQuery(conn, "SELECT * FROM reliability_interval_parameter")
weibulls_initial <- mergeWeibInput(reliability_parameter,reliability_interval,reliability_interval_parameter)
# calculate all weibulls
system.time(allweibulls <- gatherallweibulls(weibulls_initial,reliability_parameter,verbose=TRUE,unbug=FALSE,
                                            plot=TRUE,modkm=TRUE,plotdir="./plots/"))
write.csv(allweibulls,file="Apache Weibulls.csv",quote=FALSE)
# perform distribution comparison tests
source("./stattest_functions.R")
system.time(alltests <- testallweibulls(allweibulls,weibulls_initial,reliability_parameter,
                                      verbose=TRUE,doAllGroups=TRUE))
write.csv(alltests,file="Apache Tests.csv",quote=FALSE)

# calculate consequences
source("./consequence_functions.R")
conseq_initial <- mergeConsqInput(reliability_parameter,reliability_interval,reliability_interval_parameter)

# build optimization constraint matrix
constraint_matrix <- matchDistToInterval(allweibulls,weibulls_initial,reliability_parameter)
write.csv(constraint_matrix, file="Constraint Matrix.csv",quote = FALSE, row.names = TRUE)
