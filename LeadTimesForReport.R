# m777 lead times
require(RODBC)
require(lubridate)
require(dplyr)
require(ggplot2)
require(ggvis)
setwd("c:/users/tbaer/desktop/m777/data/")
pathToDB<-"P:/External Projects/3107 - ALPS M777/Data and Analysis/"
system.time(ch <- odbcConnectAccess2007(paste0(pathToDB,"inputData.accdb"))) # need to run R without being an administrator to work
sqlTables(ch, tableType = "TABLE")
system.time(MDR<-sqlFetch(ch, "MDR"))
system.time(LTfy14<-sqlQuery(ch,"SELECT MDR.REGIONAL_ACTIVITY_CODE, MDR.DATE_ORDERED, MDR.LeadTime, 'DL' AS DLnotDL
FROM MDR WHERE (((MDR.DATE_ORDERED)>=#10/1/2013#) AND ((MDR.MAINT_CATEGORY_CODE)='m') AND ((IsNull([LeadTime]))=0))
UNION ALL
SELECT MDR.REGIONAL_ACTIVITY_CODE, MDR.DATE_ORDERED, MDR.LeadTime, 'notDL' AS DLnotDL
FROM MDR WHERE (((MDR.DATE_ORDERED)>=#10/1/2013#) AND ((MDR.MAINT_CATEGORY_CODE)<>'m') AND ((IsNull([LeadTime]))=0))"))  
system.time(LTfyAll<-sqlQuery(ch,"SELECT MDR.REGIONAL_ACTIVITY_CODE, MDR.DATE_ORDERED, MDR.LeadTime, 'DL' AS DLnotDL
FROM MDR WHERE (((MDR.MAINT_CATEGORY_CODE)='m') AND ((IsNull([LeadTime]))=0))
UNION ALL
SELECT MDR.REGIONAL_ACTIVITY_CODE, MDR.DATE_ORDERED, MDR.LeadTime, 'notDL' AS DLnotDL
FROM MDR WHERE (((MDR.MAINT_CATEGORY_CODE)<>'m') AND ((IsNull([LeadTime]))=0))"))  
close(ch)

# summary stats
LTfy14 %>% group_by(DLnotDL,REGIONAL_ACTIVITY_CODE) %>% summarise(mean(LeadTime))
count(LTfy14);count(LTfy14,DLnotDL);count(LTfy14,DLnotDL,REGIONAL_ACTIVITY_CODE);count(LTfy14,REGIONAL_ACTIVITY_CODE)
# get average shipment time for each day by DL or NotDL
LT14daily<- (LTfy14 %>% group_by(DLnotDL,REGIONAL_ACTIVITY_CODE,DATE_ORDERED) %>% summarise(avgLT=mean(LeadTime)) %>% data.frame())
# sort by date
LT14daily<-LT14daily[order(LT14daily$DATE_ORDERED),]
# how many are less than 33 days?
sum(LT14daily$avgLT < 33)/nrow(LT14daily)
# plot!
g14<-ggplot(LT14daily,aes(x=DATE_ORDERED,y=avgLT,group=DLnotDL)) + geom_line(aes(colour=DLnotDL))
plot(g14)
# try monthly
LT14daily<- (LTfy14 %>% group_by(DLnotDL,REGIONAL_ACTIVITY_CODE,DATE_ORDERED) %>% summarise(avgLT=mean(LeadTime)) %>% data.frame())

# try moving average

################
# histograms
################
# all data, by MEF
par(mfrow=c(1,2))
#par(mar=c(5.1,4.1,4.1,2.1))
hist(LTfy14$LeadTime,main="M777 Fleet",xlim=c(0,200),col="gray",border="white",xlab="",breaks=30)
#axis(1,at=seq(0,200,50),labels=rep("",5))
abline(v=mean(LTfy14$LeadTime));abline(v=median(LTfy14$LeadTime))
#text(labels=bquote(underline(paste('Avg: ',~.(round(mean(LTfy14$LeadTime),0))," days"))),x=45+mean(LTfy14$LeadTime),y=1000)
text(labels=paste('Avg: ',round(mean(LTfy14$LeadTime),0)," days"),x=45+mean(LTfy14$LeadTime),y=1000)
text(labels=paste('Mdn: ',round(median(LTfy14$LeadTime),0)," days"),x=45+mean(LTfy14$LeadTime),y=650)
# I MEF
hist(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001","LeadTime"],main="I MEF",xlim=c(0,200),ylim=c(0,600),col="navyblue",border="white",xlab="",breaks=30)
abline(v=mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001","LeadTime"]));abline(v=median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001","LeadTime"]))
text(labels=paste('Avg: ',round(mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001","LeadTime"]),y=400)
text(labels=paste('Mdn: ',round(median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001","LeadTime"]),y=250)
#axis(1,at=seq(0,200,50),labels=rep("",5)) # export at 770 x 303
# II MEF
hist(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002","LeadTime"],main="II MEF",xlim=c(0,200),ylim=c(0,600),
     col="darkred",border="white",xlab="Days To Receive Part",breaks=30)
abline(v=mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002","LeadTime"]));abline(v=median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002","LeadTime"]))
text(labels=paste('Avg: ',round(mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002","LeadTime"]),y=400)
text(labels=paste('Mdn: ',round(median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002","LeadTime"]),y=250)
# III MEF
hist(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003","LeadTime"],main="III MEF",xlim=c(0,200),ylim=c(0,600),
     col="gold",border="white",xlab="Days To Receive Part",breaks=20)
abline(v=mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003","LeadTime"]));abline(v=median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003","LeadTime"]))
text(labels=paste('Avg: ',round(mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003","LeadTime"]),y=400)
text(labels=paste('Mdn: ',round(median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003","LeadTime"]),y=250)
par(mfrow=c(1,1))

# deadlining orders, by MEF
par(mfrow=c(1,2))
hist(LTfy14[LTfy14$DLnotDL == 'DL',"LeadTime"],main="M777 Fleet",xlim=c(0,200),col="gray",border="white",xlab="",breaks=20)
abline(v=mean(LTfy14[LTfy14$DLnotDL == 'DL',"LeadTime"]));abline(v=median(LTfy14[LTfy14$DLnotDL == 'DL',"LeadTime"]))
text(labels=paste('Avg: ',round(mean(LTfy14[LTfy14$DLnotDL == 'DL',"LeadTime"]),0)," days"),x=45+mean(LTfy14$LeadTime),y=60)
text(labels=paste('Mdn: ',round(median(LTfy14[LTfy14$DLnotDL == 'DL',"LeadTime"]),0)," days"),x=45+mean(LTfy14$LeadTime),y=40)
# I MEF
hist(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001" & LTfy14$DLnotDL == "DL","LeadTime"],
     main="I MEF",xlim=c(0,200),ylim=c(0,50),col="navyblue",border="white",xlab="",breaks=20)
abline(v=mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001" & LTfy14$DLnotDL == "DL","LeadTime"]))
abline(v=median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001" & LTfy14$DLnotDL == "DL","LeadTime"]))
text(labels=paste('Avg: ',round(mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001" & LTfy14$DLnotDL == "DL","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001" & LTfy14$DLnotDL == "DL","LeadTime"]),y=29)
text(labels=paste('Mdn: ',round(median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001" & LTfy14$DLnotDL == "DL","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM001" & LTfy14$DLnotDL == "DL","LeadTime"]),y=20)
# II MEF
hist(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002" & LTfy14$DLnotDL == "DL","LeadTime"],main="II MEF",xlim=c(0,200),ylim=c(0,50),
     col="darkred",border="white",xlab="Days To Receive Part",breaks=20)
abline(v=mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002" & LTfy14$DLnotDL == "DL","LeadTime"]))
abline(v=median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002" & LTfy14$DLnotDL == "DL","LeadTime"]))
text(labels=paste('Avg: ',round(mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002" & LTfy14$DLnotDL == "DL","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002" & LTfy14$DLnotDL == "DL","LeadTime"]),y=29)
text(labels=paste('Mdn: ',round(median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002" & LTfy14$DLnotDL == "DL","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM002" & LTfy14$DLnotDL == "DL","LeadTime"]),y=20)
# III MEF
hist(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003" & LTfy14$DLnotDL == "DL","LeadTime"],main="III MEF",xlim=c(0,200),ylim=c(0,50),
     col="gold",border="white",xlab="Days To Receive Part",breaks=20)
abline(v=mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003" & LTfy14$DLnotDL == "DL","LeadTime"]))
abline(v=median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003" & LTfy14$DLnotDL == "DL","LeadTime"]))
text(labels=paste('Avg: ',round(mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003" & LTfy14$DLnotDL == "DL","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003" & LTfy14$DLnotDL == "DL","LeadTime"]),y=29)
text(labels=paste('Mdn: ',round(median(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003" & LTfy14$DLnotDL == "DL","LeadTime"]),0)," days")
     ,x=45+mean(LTfy14[LTfy14$REGIONAL_ACTIVITY_CODE == "MIM003" & LTfy14$DLnotDL == "DL","LeadTime"]),y=20)
par(mfrow=c(1,1))