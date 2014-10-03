help(install.packages)
.libPaths()
install.packages("knitr",lib="C:\\Program Files\\R\\R-2.15.2\\library")
install.packages("knitr")

require("xts")
require("XML")
setwd("C:\\users\\tbaer\\Desktop")
(simpledf<-xmlToDataFrame("generic_file.xml",colClasses=c("character","integer","integer","integer"),collectNames=FALSE))
str(simpledf)
# made my newTOW_Loc into an xml file by exporting from access
NTLxml<-xmlToDataFrame("C:\\Users\\tbaer\\Desktop\\cbm\\october redo weibulls\\xml test\\newTOW_LOC_reset_at_repair.xml",
                       colClasses=c("character","character","character","character","character","character","character","numeric",
                                    "integer","integer"),stringsAsFactors=TRUE,collectNames=FALSE)
NTLxml$MaxOfINIT_DT<-as.Date(NTLxml$MaxOfINIT_DT)
head(NTLxml)
str(NTLxml)