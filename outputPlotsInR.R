## plot availability
setwd("C:/Users/tbaer/Desktop/hmmwv v&v/models")
avails<-read.csv("avail.csv")
avails$Scenario<-as.factor(avails$Scenario)
avails$MEF<-as.factor(avails$MEF)
avails$YearQtr<-as.character(avails$YearQtr)
require(ggplot2)
a <- ggplot(data=avails, aes(colour=Scenario)) + 
  geom_line(aes(x=YearQtr,y=avail))
plot(a)
plot(a+ facet_grid(. ~ MEF))

#from clipboard
a<-read.table("clipboard",sep="\t")

#from insight
require(RODBC)
#require(ggplot2)
simid = 100 # 1000
#1 d/l the mysql driver for local odbc
#2 use admin tools to create a dsn with these drivers (must specify a schema; here, output)
  #> control panel -> system & security -> administrative tools -> data sources -> add
   #> select "MySQL ODBC 5.3 Unicode Driver"
   #> give connection a name, server-localhost; enter password, specify output schema 
cnF <- odbcConnect(dsn="Local3306output",uid="root",pw="password") # 3306
#cnF <- odbcConnect(dsn="Local3307output",uid="root",pw="") # 3307
sqlTables(cnF)
# a big query - avail <- sqlFetch(cn, "availability")
avgAssetAvailFQ <- sqlQuery(cnF, paste0("SELECT timestamp as Date, avg(value) as avail FROM
                          availability WHERE asset = 1 and interval_unit_name like 'CALENDAR_QUARTER' and simulation_id = ",simid," GROUP BY timestamp"))
plot(avgAssetAvailFQ, type = "l")

avgSpareAvailabilityTD <- sqlQuery(cn, paste0("SELECT timestamp, avg(value) FROM spare_availability 
                                 WHERE simulation_id = ",simid," GROUP BY timestamp"))
plot(avgSpareAvailabilityTD, type = "l")
spareQuantityByTypeTD <- sqlQuery(cn, "SELECT")

avgTotalQtrFailures <- sqlQuery(cn, paste0("SELECT timestamp, avg(value) FROM
                          component_events WHERE event_name = 'Failure' and simulation_id = ",
                                            simid," GROUP BY timestamp"))
plot(avgTotalQtrFailures, type = "l")


# RMySQL - this package is a mess
require(RMySQL)
cn <- dbConnect(MySQL(), user='root',port=3307,host='localhost',dbname='output')
avgAssetAvail <- dbSendQuery(cn, paste0("SELECT timestamp, avg(value) FROM
                          availability WHERE asset = 1 and simulation_id = ",simid," GROUP BY timestamp"))
avgAssetAvail <- dbFetch(avgAssetAvail)
plot(avgAssetAvail, type = "l")


## from DEMAND Pro
aPath<-"P:/Internal Projects/Data Scientist Team/InsightLCM/Testing/FAST/DEMAND Pro Basic Training/BasicCourseModelsDEMAND/PreEx1/"
#aPath<-"C:/Users/tbaer/Desktop/"
cnD <- odbcConnectAccess2007(access.file=paste0(aPath,"Model 0-01.mdb"))
#cnD <- odbcConnectAccess2007(access.file=paste0(aPath,"test.mdb"))
sqlTables(cnD)
# this query converts the year and quarter to a timestamp using weeks and dates table
avgAssetAvailDQ <- sqlQuery(cnD, "SELECT [*Weeks and Dates].Date, Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed]) AS avail
FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([*Weeks and Dates].Quarter = [out Availability].Qtr) AND ([out Availability].Year = [*Weeks and Dates].Year)
WHERE ((([*Weeks and Dates].Week)=1))
GROUP BY [*Weeks and Dates].Date, [out Availability].Year, [out Availability].Qtr")
plot(avgAssetAvailDQ, type = "l")


## Now plot both
plot(x=avgAssetAvailDQ$Date,y=avgAssetAvailDQ$avail, type = "l",xlab="Date",ylab="Asset Avail") # started in FY2007 Q1
lines(x=avgAssetAvailFQ$Date,y=avgAssetAvailFQ$avail/100, col="Red")
legend(x="topright",legend=c("DEMAND","FAST"),fill=c("Black","Red"))
