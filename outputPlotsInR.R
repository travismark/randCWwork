#from insight
require(RODBC)
#require(ggplot2)
simid = 100 # 1000
#1 d/l the mysql driver for local odbc
#2 use admin tools to create a dsn with these drivers (must specify a schema; here, output)
  #> control panel -> system & security -> administrative tools -> data sources -> add
   #> select "MySQL ODBC 5.3 Unicode Driver"
   #> give connection a name, server-localhost; enter password, specify output schema 

#### From FAST
cnF <- odbcConnect(dsn="Local3306output",uid="root",pw="password") # 3306
#cnF <- odbcConnect(dsn="Local3307output",uid="root",pw="") # 3307
sqlTables(cnF)
## Availability
# Weekly
avgAssetAvailFW <- sqlQuery(cnF, paste0("SELECT timestamp as Date, avg(value) as avail FROM
                          availability WHERE asset = 1 and interval_unit_name like 'CALENDAR_WEEK' and simulation_id = ",simid," GROUP BY timestamp"))
plot(avgAssetAvailFW, type = "l")
# Weekly-Average SD
(sqrt(var(avgAssetAvailFW$avail)))
# Overall
(avgAssetAvailFO <- sqlQuery(cnF, paste0("SELECT timestamp as Date, avg(value) as avail FROM
                          availability WHERE asset = 1 and interval_unit_name like 'OVERALL' and simulation_id = ",simid))) # Overall
## Failures
avgTotalQtrFailuresFQ <- sqlQuery(cnF, paste0("SELECT timestamp as Date, avg(value) as value FROM
                          component_events WHERE event_name = 'Failure' and interval_unit_name like 'CALENDAR_QUARTER' and simulation_id = ",
                                              simid," GROUP BY timestamp"))
plot(avgTotalQtrFailuresFQ, type = "l")
(avgTotalQtrFailuresFO <- sqlQuery(cnF, paste0("SELECT timestamp as Date, avg(value) as value FROM
                          component_events WHERE event_name = 'Failure' and interval_unit_name like 'OVERALL' and simulation_id = ",simid)))
## Spare Availability
avgSpareAvailabilityTD <- sqlQuery(cn, paste0("SELECT timestamp, avg(value) FROM spare_availability 
                                 WHERE simulation_id = ",simid," GROUP BY timestamp"))
plot(avgSpareAvailabilityTD, type = "l")
spareQuantityByTypeTD <- sqlQuery(cn, "SELECT")

#### From DEMAND Pro
aPath<-"P:/Internal Projects/Data Scientist Team/InsightLCM/Testing/FAST/DEMAND Pro Basic Training/BasicCourseModelsDEMAND/PreEx1/"
#aPath<-"C:/Users/tbaer/Desktop/"
cnD <- odbcConnectAccess2007(access.file=paste0(aPath,"Model 1-1_TS1.mdb"))
#cnD <- odbcConnectAccess2007(access.file=paste0(aPath,"test.mdb"))
sqlTables(cnD)
## Availability
# this query converts the year and quarter to a timestamp using weeks and dates table
# Weekly
avgAssetAvailDW <- sqlQuery(cnD, "SELECT [*Weeks and Dates].Date, Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed]) AS avail
FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([out Availability].Week = [*Weeks and Dates].Week) AND ([out Availability].Qtr = [*Weeks and Dates].Quarter) AND ([out Availability].Year = [*Weeks and Dates].Year)
GROUP BY [*Weeks and Dates].Date, [out Availability].Year, [out Availability].Qtr")
plot(avgAssetAvailDW, type = "l")
# Weekly-Average SD
sqrt(var(avgAssetAvailDW$avail*100)) # times 100 to match with FAST
# Overall
(avgAssetAvailDO <- sqlQuery(cnD, "SELECT Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed]) AS avail, Avg([Avail SD]*[Availability]/100) AS SD FROM [out Availability]"))
#(SDavgAssetAvailDW <- sqlQuery(cnD, "SELECT sqr(var(src.avail)) AS VarOfavail
#FROM (SELECT [*Weeks and Dates].Date, Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed]) AS avail
#FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([out Availability].Week = [*Weeks and Dates].Week) AND ([out Availability].Qtr = [*Weeks and Dates].Quarter) AND ([out Availability].Year = [*Weeks and Dates].Year)
#GROUP BY [*Weeks and Dates].Date, [out Availability].Year, [out Availability].Qtr)  AS src")) 

avgAssetAvailDQ <- sqlQuery(cnD, "SELECT [*Weeks and Dates].Date, Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed]) AS avail
FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([*Weeks and Dates].Quarter = [out Availability].Qtr) AND ([out Availability].Year = [*Weeks and Dates].Year)
WHERE ((([*Weeks and Dates].Week)=1)) GROUP BY [*Weeks and Dates].Date, [out Availability].Year, [out Availability].Qtr, [out Availability].Week")


avgTotalQtrFailuresDQ <- sqlQuery(cnD, "SELECT [*Weeks and Dates].Date, Sum([out LRU events].UER) AS SumOfUER
FROM [out LRU events] INNER JOIN [*Weeks and Dates] ON ([out LRU events].Year = [*Weeks and Dates].Year) AND ([out LRU events].Qtr = [*Weeks and Dates].Quarter) WHERE ((([*Weeks and Dates].Week)=1)) GROUP BY [*Weeks and Dates].Date")
plot(avgTotalQtrFailuresDQ, type = "l")


### Now plot both
## Availability
# Weekly
plot(x=avgAssetAvailDW$Date,y=avgAssetAvailDW$avail*100, type = "l",xlab="Date",ylab="Asset Avail") # started in FY2007 Q1
lines(x=avgAssetAvailFW$Date,y=avgAssetAvailFW$avail, col="Red")
#par(xpd=TRUE)
#legend(x="topright",legend=c("DEMAND","FAST"),fill=c("Black","Red"), inset=c(0,-.2))
legend(x="topright",legend=c("DEMAND","FAST"),fill=c("Black","Red"))
#par(xpd=FALSE)
# Weekly Histogram
par(mfcol=c(1,2))
a<-hist(avgAssetAvailDW$avail*100,col="black",border="white")
b<-hist(avgAssetAvailFW$avail,col="red",border="black")
par(mfcol=c(1,1))
# Failures
plot(x=avgTotalQtrFailuresFQ$Date,y=avgTotalQtrFailuresFQ$value, col="Red", type = "l",xlab="Date",ylab="Total Failures") # started in FY2007 Q1
lines(x=avgTotalQtrFailuresDQ$Date,y=avgTotalQtrFailuresDQ$SumOfUER, col="Black")
legend(x="topright",legend=c("DEMAND","FAST"),fill=c("Black","Red"))
