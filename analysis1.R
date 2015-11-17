# University of Dayton Research Institute
# Autumn 2015
require(RODBC);require(lubridate);require(ggplot2);require(tm);require(dplyr);require(tidyr);require(plotly)
safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))
Sys.setenv("plotly_username"="data_scientist")
Sys.setenv("plotly_api_key"="kj73ou8nt3")
udri <- odbcConnect(dsn="onion-udri",uid="tbaer",pw="tbaer1") # created through the Data Sources (ODBC) window described above
## Location Data
geoLoc <- sqlQuery(udri, "SELECT * from geoloc_code")
names(geoLoc) <- c("Geographic_Location","Location_Name","City","State")
## Debrief Data
debrief <- sqlQuery(udri, "SELECT * FROM debrief")
# add and clean data
# de-duplicate
debrief <- distinct(debrief)
# adjust column types
debrief$Sortie_Date <- ymd(debrief$Sortie_Date, tz='UTC')
debrief$Landing_Date <- ymd(debrief$Landing_Date, tz='UTC')
debrief$Takeoff_Date <- ymd(debrief$Takeoff_Date, tz='UTC')
debrief$Landing_Status <- as.factor(debrief$Landing_Status)
debrief$Sortie_Modifier <- as.factor(debrief$Sortie_Modifier)
debrief$Capability_Code <- as.factor(debrief$Capability_Code)
debrief$Serial_Number <- as.factor(debrief$Serial_Number)
debrief$Job_Control_Number <- as.factor(debrief$Job_Control_Number)
debrief$Discrepancy_Narrative <- as.character(debrief$Discrepancy_Narrative)
debrief$Deviation_Remarks <- as.character(debrief$Deviation_Remarks)
# add day information
debrief$Sortie_DayOfWeek <- wday(debrief$Sortie_Date, label = TRUE)
# turn WUC into a factor that matches oem data (take out the ".0" at the end)
debrief$Work_Unit_Code <- as.character(debrief$Work_Unit_Code)
debrief$Work_Unit_Code <- gsub("\\.0","",debrief$Work_Unit_Code)
# there are still 3 and 4 digit WUCs.  these will partially match to OEM data
# so don't adjust these
debrief$Work_Unit_Code <- factor(debrief$Work_Unit_Code)
# turn Subsystem WUC into a factor that matches oem data (take out the ".0" at the end)
debrief$Subsystem_Work_Unit_Code <- as.character(debrief$Subsystem_Work_Unit_Code)
debrief$Subsystem_Work_Unit_Code <- gsub("\\.0","",debrief$Subsystem_Work_Unit_Code)
debrief$Subsystem_Work_Unit_Code <- factor(debrief$Subsystem_Work_Unit_Code)

# fix mission code
# take out spaces and hyphens
debrief$Mission_Code <- as.character(debrief$Mission_Code)
debrief$Mission_Code <- gsub(pattern="-",replacement="",x=debrief$Mission_Code)
debrief$Mission_Code <- gsub(pattern=" ",replacement="",x=debrief$Mission_Code)
debrief$Mission_Code <- factor(debrief$Mission_Code)
levels(debrief$Mission_Code)[3] <- "450" # change "450.0" to "450" fix the mission code in debrief
# add mission classification
# make some classes - ten of these including "other" done by outside help picking the top nine categories and seeing where some codes were close enough to the class to estimate
mc <- data.frame("Mission_Code"=unique(as.character(debrief$Mission_Code)),"Mission_Class"=NA)
# mc[grep("BONE", mc$code),]$class <- "BONE" # 1
for (className in c("BONE","DARK","FELON","FIEND","HAWK","PUMA","SLAM","SLAYER")){
  mc[grep(className, mc$Mission_Code),]$Mission_Class <- className
}
# fix a few more
mc[grep("TH", mc$Mission_Code),]$Mission_Class <- "THUNDER"; mc[grep("DR", mc$Mission_Code),]$Mission_Class <- "THUNDER"; mc[grep("TR", mc$Mission_Code),]$Mission_Class <- "THUNDER"
mc[grep("FN", mc$Mission_Code),]$Mission_Class <- "FIEND";
# set NAs to class "OTHER" where code isn't null
mc[!is.na(mc$Mission_Code) & is.na(mc$Mission_Class),]$Mission_Class <- "OTHER"
# set to factor
mc$Mission_Class <- factor(mc$Mission_Class)
# this classifies most of the longest (>15hr) missions as "Other", along with a few shorter missions 
setwd("C:/Users/tbaer/Desktop/udri")
#mc<-read.csv("mission_class.csv")
debrief<-left_join(debrief,mc,by="Mission_Code")
# add an NA
debrief$Mission_Class<-addNA(debrief$Mission_Class)
# set NAs to Other - not really a good idea
# levels(debrief$Mission_Class)<-c(levels(debrief$Mission_Class)[1:length(levels(debrief$Mission_Class))-1],"Other")
# input data has been reset to ten total factors, excluding "Other"
# mission class vs geo loc (rows)
(ggplot(debrief, aes(x=Mission_Class, fill=Geographic_Location))+geom_histogram()+coord_flip())+ylab("debrief rows")
# sorties-ish
sts <- group_by(debrief, Sortie_Number, Sortie_Date, Mission_Class, Geographic_Location) %>% distinct()
(ggplot(sts, aes(x=Mission_Class, fill=Geographic_Location))+geom_histogram()+coord_flip())+ylab("sorties-ish")

# main organizational data
print("Command");table(debrief$Command);round(table(debrief$Command)/nrow(debrief),3)
print("Geographic Location");table(debrief$Geographic_Location);round(table(debrief$Geographic_Location)/nrow(debrief),3)
print("Organization");table(debrief$Organization);round(table(debrief$Organization)/nrow(debrief),3)
length(unique(debrief$Job_Control_Number))/nrow(debrief)

# time and date figures
qplot(debrief$Sortie_Date)
qplot(debrief$Sortie_DayOfWeek)
qplot(debrief$Flight_Duration) # three groups of durations
qplot(debrief$Flight_Duration, fill=debrief$Sortie_DayOfWeek)
qplot(debrief$Flight_Duration, fill=debrief$Geographic_Location)
qplot(debrief$Flight_Duration, fill=debrief$Mission_Class)
qplot(debrief$Flight_Duration, fill=debrief$Deviation_Code)
(ggplot(debrief[debrief$Geographic_Location %in% "FXBM",], aes(x=Flight_Duration))+geom_histogram(aes(fill=Deviation_Code))) 
(ggplot(debrief[debrief$Mission_Class %in% "BONE",], aes(x=Flight_Duration))+geom_histogram(aes(fill=Deviation_Code))) # not many deviations
qplot(Flight_Duration, data=debrief[debrief$Sortie_DayOfWeek=="Sun",]) # Sunday flights are usually long
qplot(Flight_Duration, data=debrief[debrief$Sortie_DayOfWeek=="Sat",]) # Saturday flights are often short, and sometimes long
qplot(Flight_Duration, data=debrief[debrief$Sortie_DayOfWeek=="Wed",]) # no real insight
qplot(debrief$Takeoff_Time/100, fill = debrief$Sortie_DayOfWeek)

qplot(debrief$Work_Unit_Code) # all WUC
# top ten WUCsd
head(sort(table(debrief$Work_Unit_Code),decreasing=T),10)
ttw<-names(tail(sort(table(debrief$Work_Unit_Code)),10))
barplot(rev(tail(sort(table(debrief$Work_Unit_Code)),10)), main = "Top Ten WUC by Records")
unique(debrief[debrief$Work_Unit_Code %in% ttw, "WUC_Description"])
qplot(debrief[debrief$Work_Unit_Code %in% ttw,"Work_Unit_Code"])

# What is Sortie Number?
plot(y=debrief$Sortie_Number, x=debrief$Sortie_Date)
qplot(y=Sortie_Number, x=Sortie_Date, data = debrief, colour=Geographic_Location)
qplot(y=Sortie_Number, x=Sortie_Date, data = debrief, colour=Organization)
qplot(y=Sortie_Number, x=Sortie_Date, data = debrief, colour=Mission_Class)
qplot(y=Sortie_Number, x=Sortie_Date, 
      data = debrief[debrief$Serial_Number %in% c(8600000119,8500000074,8500000080,8600000105,8600000097),], colour=Serial_Number)

## Sorties with Aborts or Other Deviation Codes
sortieStatuses <- group_by(debrief, Sortie_Date, Sortie_Number, Serial_Number, Geographic_Location) %>% 
  select(Sortie_Date, Sortie_Number, Serial_Number, Cause_Code, Geographic_Location) %>% distinct()
sortieStatuses <- left_join(sortieStatuses, geoLoc)
# Deviation by type - Air Abort, Ground Abort, Flight Emergency, Tail Swap / Spare Aircraft, Other Deviation, or No Deviation
sortieStatuses$Mission_Result <- "No Deviation"
deviatedSorties <- filter(debrief,!is.na(Deviation_Code)) %>% select(Sortie_Date, Sortie_Number, Serial_Number) %>% distinct()
deviatedSorties$Deviation <- "Other Deviation"
# other
sortieStatuses <- left_join(sortieStatuses,deviatedSorties)
sortieStatuses[!(is.na(sortieStatuses$Deviation)),]$Mission_Result <- "Other Deviation"
sortieStatuses$Deviation <- NULL
# cancelations
deviatedSorties <- filter(debrief,Deviation_Code %in% "CX") %>% select(Sortie_Date, Sortie_Number, Serial_Number) %>% distinct()
deviatedSorties$Deviation <- "Cancellation"
sortieStatuses <- left_join(sortieStatuses,deviatedSorties)
sortieStatuses[sortieStatuses$Deviation %in% "Cancellation",]$Mission_Result <- "Cancellation"
sortieStatuses$Deviation <- NULL
# spare / ts
deviatedSorties <- filter(debrief,Deviation_Code %in% c("TS","SP")) %>% select(Sortie_Date, Sortie_Number, Serial_Number) %>% distinct()
deviatedSorties$Deviation <- "Spare or Swap"
sortieStatuses <- left_join(sortieStatuses,deviatedSorties)
sortieStatuses[sortieStatuses$Deviation %in% "Spare or Swap",]$Mission_Result <- "Spare or Swap"
sortieStatuses$Deviation <- NULL
# flight emergency
deviatedSorties <- filter(debrief,Deviation_Code %in% "FE") %>% select(Sortie_Date, Sortie_Number, Serial_Number) %>% distinct()
deviatedSorties$Deviation <- "Flight Emergency"
sortieStatuses <- left_join(sortieStatuses,deviatedSorties)
sortieStatuses[sortieStatuses$Deviation %in% "Flight Emergency",]$Mission_Result <- "Flight Emergency"
sortieStatuses$Deviation <- NULL
# ground
deviatedSorties <- filter(debrief,Deviation_Code %in% "GA") %>% select(Sortie_Date, Sortie_Number, Serial_Number) %>% distinct()
deviatedSorties$Deviation <- "Ground Abort"
sortieStatuses <- left_join(sortieStatuses,deviatedSorties)
sortieStatuses[sortieStatuses$Deviation %in% "Ground Abort",]$Mission_Result <- "Ground Abort"
sortieStatuses$Deviation <- NULL
# air abort
deviatedSorties <- filter(debrief,Deviation_Code %in% c("AA","AI")) %>% select(Sortie_Date, Sortie_Number, Serial_Number) %>% distinct()
deviatedSorties$Deviation <- "Air Abort"
sortieStatuses <- left_join(sortieStatuses,deviatedSorties)
sortieStatuses[sortieStatuses$Deviation %in% "Air Abort",]$Mission_Result <- "Air Abort"
sortieStatuses$Deviation <- NULL
# convert to ordered factors
sortieStatuses$Mission_Result <- factor(sortieStatuses$Mission_Result,levels = unique(sortieStatuses$Mission_Result)[c(4,6,7,1,5,3,2)])
( gfs <- ggplot(sortieStatuses,aes(x="",fill=Mission_Result)) + geom_bar(stat="bin") + coord_flip() + 
  theme_bw() + labs(title="Sortie Status For June & July 2014",y="Attempted Sorties",x="") + 
  scale_fill_manual(values=c("firebrick","orange","khaki","orchid4","grey44","lightblue","palegreen2")) +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) )
ggsave(filename="attempted_mission_status.svg", plot=gfs, width=8, height=2, scale=1)
( gfs <- ggplot(sortieStatuses,aes(x=Location_Name,fill=Mission_Result)) + geom_bar(stat="bin") + coord_flip() + 
  theme_bw() + labs(title="Sortie Status For June & July 2014",y="Attempted Sorties",x="Geographic Location") + 
  scale_fill_manual(values=c("firebrick","orange","khaki","orchid4","grey44","lightblue","palegreen2")) +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) )
ggsave(filename="attempted_mission_status_byGeoLoc.svg", plot=gfs, width=8, height=2, scale=1)
sortieStatusesForPlotly <- group_by(sortieStatuses, Mission_Result, Location_Name) %>% summarise(count = n())
sortieStatusesForPlotly$Mission_Result <- factor(sortieStatusesForPlotly$Mission_Result, levels = rev(levels(sortieStatusesForPlotly$Mission_Result)) )
pal <- RColorBrewer::brewer.pal(nlevels(sortieStatusesForPlotly$Mission_Result), "Set2")
pal <- rev(c("firebrick","orange","khaki","orchid4","grey44","lightblue","palegreen2"))
( gfsp <- plot_ly(sortieStatusesForPlotly, y=Location_Name,x=count,color=Mission_Result, colors=pal, type="bar",orientation="h") %>% 
  layout(barmode="stack",margin=list("l"=160,"r"=0,"t"=0,"b"=60),
         xaxis=list(title="Attempted Sorties"), yaxis=list(title="")) )
plotly_POST(gfsp,filename="sortieDeviation",world_readable=FALSE,fileopt="overwrite")
## Deviations by Cause Code - each sortie has only a single cause code
# take first two digits of cause code - these all MT, GA, and AI are maintenance-related)
sortieStatuses$Cause_Code <- substr(sortieStatuses$Cause_Code,1,2)

## text analysis
# two text fields: Discrepancy Narrative and Deviation Remarks
# first pull out the '[Code]' - if fails, try '(Code)' but sometimes these are bad systems
#gregexpr("\\]","tele[pho]ne")[[1]][1]
getCode<-function(s){
  firstTry <- unlist(strsplit(unlist(strsplit(s,"\\["))[2],"\\]"))[1] # get text between [%%%]
  if(is.na(firstTry)) { # didn't match
    return(unlist(strsplit(unlist(strsplit(s,"\\("))[2],"\\)"))[1]) # get text between (%%%)
  }
  return(firstTry) # if the first equation found something
}
debrief$Discrepancy_Narrative_System <- sapply(debrief$Discrepancy_Narrative,FUN=getCode)
# some of these aren't really systems, so take them out
ones<-group_by(debrief,Discrepancy_Narrative_System)
ones<-summarise(ones,c = n())
ones<-ones[ones$c==1,]$Discrepancy_Narrative_System
debrief$Discrepancy_Narrative_System[debrief$Discrepancy_Narrative_System %in% (ones)]<-NA
# link a few that got mismatched
debrief$Discrepancy_Narrative_System[debrief$Discrepancy_Narrative_System %in% ("OAS7")]<-"OAS"
debrief$Discrepancy_Narrative_System[debrief$Discrepancy_Narrative_System %in% ("FUELS")]<-"FUEL"
# now set to factor - only 25 systems
debrief$Discrepancy_Narrative_System<-factor(debrief$Discrepancy_Narrative_System)
qplot(debrief$Discrepancy_Narrative_System)
nonNAdebrief<-debrief[!is.na(debrief$Discrepancy_Narrative_System),]
qplot(nonNAdebrief$Discrepancy_Narrative_System) # un sorted
# sort (thanks to https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph) - for pareto
discSystem<-(nonNAdebrief$Discrepancy_Narrative_System)
discSystem <- factor(discSystem, levels = names(sort(table(discSystem),decreasing=TRUE)))
qplot(discSystem) # sorted
discSystemSummary <- group_by(nonNAdebrief, Discrepancy_Narrative_System)
discSystemSummary <- summarise(discSystemSummary, count = n())
# fancier gg plot, needs count as a separate field
discSystemSummary <- within(discSystemSummary, Discrepancy_Narrative_System <- factor(
  Discrepancy_Narrative_System, levels = rev(levels(discSystem))))
gb <- ggplot(discSystemSummary, aes(x=Discrepancy_Narrative_System, y=count)) + 
  geom_bar(stat="identity") + coord_flip()
gb
gp <- ggplot(discSystemSummary, aes(y=Discrepancy_Narrative_System, x=count)) + 
  geom_point(stat="identity")
gp
ggsave(filename="system_from_debrief_discrepancy_narrative.svg", plot=gp, scale=2)
# Is the discrepancy narrative system different than the wuc subsystem?
narSysCount <- group_by(debrief, Discrepancy_Narrative_System) %>% summarise(narSysCount = n())
subSysCount <- group_by(debrief, Subsystem_WUC_Description) %>% summarise(subSysCount = n())
WUCsubsysVNarSys <- group_by(debrief, Discrepancy_Narrative_System, Subsystem_WUC_Description) %>% summarise(subSysNarSysCount = n())
WUCsubsysVNarSys <- inner_join(WUCsubsysVNarSys, narSysCount, by="Discrepancy_Narrative_System")
WUCsubsysVNarSys <- inner_join(WUCsubsysVNarSys, subSysCount, by="Subsystem_WUC_Description")

# only OAS discrepancy system, ordered by subsystem
WUCsubsysVNarSys[order(-WUCsubsysVNarSys$subSysNarSysCount),][WUCsubsysVNarSys[order(-WUCsubsysVNarSys$subSysNarSysCount),]$Discrepancy_Narrative_System %in% 'OAS',]
# there doesn't appear to be a strong correlation between the system in discrepancy narrative and the wuc subsystem

# try some advanced text stuff
# discrepancy narrative
debriefDiscrepancyVector <- VectorSource(debrief$Discrepancy_Narrative) # convert to vector of words
debriefDiscrepancyCorpus <- Corpus(debriefDiscrepancyVector) # convert to corpus
#str(documentCorpus)
debriefDiscrepancyCorpus <- tm_map(debriefDiscrepancyCorpus, removeWords, stopwords("english")) # remove stop words
# das - data analysis system
require(wordcloud);wordcloud(debriefDiscrepancyCorpus,max.words=100)
# deviation remarks
debriefDeviationVector <- VectorSource(debrief$Deviation_Remarks)
debriefDeviationVector <- Corpus(debriefDeviationVector)
debriefDeviationVector <- tm_map(debriefDeviationVector, removeWords, stopwords("english"))
wordcloud(debriefDeviationVector,max.words=100)

## maps
require(maps)
name<-c("Dyess AFB", "Ellsworth AFB", "Edwards AFB", "Tinker AFB")
lat<-c(32.420833,44.146389,34.905556,35.414722)
long<-c(-99.854722,-103.074722,-117.883611,-97.386667)
geo_locs<-data.frame(name,lat,long)
map(database = "usa")
# ?

## achieved vs. scheduled flight hours
missionCdHrs <- group_by(debrief, Mission_Code, Mission_Class)
missionCdMaxHrs <- summarise(missionCdHrs, maxFlDur = max(Flight_Duration))
missionCdMaxHrs <- arrange(missionCdMaxHrs, maxFlDur)
(mchr <- ggplot(missionCdMaxHrs, aes(x=Mission_Code, y=maxFlDur)) + geom_point(size=4,aes(colour=Mission_Class)))
( mchr <- ggplot(missionCdMaxHrs, aes(x=Mission_Code, y=maxFlDur)) + geom_point(size=4) + facet_grid(Mission_Class~.) )
# mission class density
(mcHrDen <- ggplot(missionCdMaxHrs, aes(x=maxFlDur)) + geom_density(alpha=0.4,aes(fill=Mission_Class)))
(ggplot(filter(missionCdMaxHrs,Mission_Class %in% "BONE"), aes(maxFlDur)) + geom_density())
# for all aborted sorties (air and ground) - calculate lost hours (scheduled - actual) 
#  assuming scheduled hours are the average for that mission code where deviation code isn't aborted
#  or substituted (TS) or spare (SP) or cancelled (CX) and flight duration > 0 (includes missing Deviation Codes)
goodSortieHrs <- filter(debrief, Flight_Duration > 0, !(Deviation_Code %in% c('GA','AI','AA','TS','CX','SP')))
goodSortieHrs <- group_by(goodSortieHrs, Sortie_Number, Sortie_Date, Mission_Code, Mission_Class, Flight_Duration, Serial_Number) %>% distinct()
goodSortieHrs <- ungroup(goodSortieHrs) %>% select(Mission_Code, Mission_Class, Flight_Duration) #%>% summarise(avgFlHr = mean(Flight_Duration), sortieCount = n())
medMisCodeHrs <- group_by(goodSortieHrs, Mission_Code, Mission_Class) %>% summarise(count = n(), medCodeFltDur = median(Flight_Duration))
medMisClassHrs <- group_by(goodSortieHrs, Mission_Class) %>% summarise(count = n(), medClassFltDur = median(Flight_Duration))
medMisAllHrs <- median(goodSortieHrs$Flight_Duration) # 4.2
avgMisAllHrs <- mean(goodSortieHrs$Flight_Duration) # 6.2
qplot(goodSortieHrs$Flight_Duration, fill=goodSortieHrs$Mission_Class)

abortedSorties <- filter(debrief, Deviation_Code %in% c('GA', 'AI', 'AA')) %>% select(Mission_Code, Mission_Class, Flight_Duration, Serial_Number, Sortie_Number, Sortie_Date) 
abortedSorties <- group_by(abortedSorties, Mission_Code, Mission_Class, Flight_Duration, Serial_Number,Sortie_Number, Sortie_Date) %>% distinct()
# abortedSorties <- abortedSorties[!is.na(abortedSorties$Mission_Code),]# drop the NA - unnecessary
# merge with average code and class 
  # 1) only use code if more than two sorties, (34% # sum(table(medMisCodeHrs$count)[1:2]) / sum(table(medMisCodeHrs$count)) )
  # 2) assign NAs to the median of all missions in avgMisCodeHrs
medMisCodeHrs <- filter(medMisCodeHrs, count>2)
abortedSorties <- left_join(abortedSorties, select(medMisCodeHrs, Mission_Code, medCodeFltDur), by="Mission_Code")
abortedSorties <- left_join(abortedSorties, select(medMisClassHrs, Mission_Class, medClassFltDur), by="Mission_Class")
abortedSorties$diff <- medMisAllHrs # will stay with those sorties with no mission code
abortedSorties[!is.na(abortedSorties$medCodeFltDur),]$diff <- pmax(0,abortedSorties[!is.na(abortedSorties$medCodeFltDur),]$medCodeFltDur-abortedSorties[!is.na(abortedSorties$medCodeFltDur),]$Flight_Duration)
abortedSorties[is.na(abortedSorties$medCodeFltDur),]$diff <- pmax(0,abortedSorties[is.na(abortedSorties$medCodeFltDur),]$medClassFltDur-abortedSorties[is.na(abortedSorties$medCodeFltDur),]$Flight_Duration)
(missingHrs <- sum(abortedSorties$diff))
# by tail number
# at least one tail aborts two flights in the same day ! 8600000121 on June 12th (no JCNs created) - should I count this twice?
abortedSortiesByTNsomeAborts <- group_by(abortedSorties, Serial_Number, Mission_Class, Sortie_Date) %>% summarise(missedHours = sum(diff))
missedHoursByTN <- data.frame("Serial_Number"=levels(abortedSortiesByTNsomeAborts$Serial_Number))
missedHoursByTN <- left_join(missedHoursByTN, abortedSortiesByTNsomeAborts, by="Serial_Number")
missedHoursByTN[is.na(missedHoursByTN$missedHours),"missedHours"]<-0 # replace NAs with 0
(gsimple <- ggplot(missedHoursByTN, aes(x=Serial_Number, y=missedHours)) + geom_bar(stat="identity") + coord_flip()) + ylab("Missed Flight Hours due to Aborts")
# could color bars by air/ground, facet for two geographic locations, etc.

# how many achieved hours?
sortieHrs <- group_by(debrief, Sortie_Number, Serial_Number, Sortie_Date, Sortie_DayOfWeek, Mission_Code, Mission_Class) # includes aborted, cancelled, tail-swap, etc. (sometimes mutliple SNs per sortie)
sortieHrs <- summarise(sortieHrs, maxFlHr = max(Flight_Duration), records = n())
(achievedHrs <- sum(sortieHrs$maxFlHr) )

# missed / (missed+achieved) hours
missingHrs / (missingHrs + achievedHrs) # unavailability

# achieved hours by tail
achievedHrsByTN <- group_by(debrief, Sortie_Number, Sortie_Date, Serial_Number, Mission_Class, Flight_Duration) %>% summarise(achievedSortieHours = max(Flight_Duration)) %>% group_by(Serial_Number, Mission_Class, Sortie_Date) %>% summarise(achievedHours = sum(achievedSortieHours))
flightHrsByTN <- left_join(achievedHrsByTN,missedHoursByTN,by=c("Serial_Number","Mission_Class","Sortie_Date"))
# flip the data
flightHrsByTN <- gather(flightHrsByTN,"flightType","flightHours",achievedHours:missedHours)
(gtn <- ggplot(flightHrsByTN, aes(x=Serial_Number,y=flightHours)) + geom_bar(stat="identity", aes(fill=flightType)) + coord_flip() + labs(y="Flight Hours") + ggtitle("Flight Hours by Tail Number, Achieved or Missed"))
ggsave(filename="tail_number_flight_hours_byMissedAchieved.svg", plot=gtn, width=14, height=10, scale=1)
(gtn <- ggplot(flightHrsByTN, aes(x=Serial_Number,y=flightHours)) + geom_bar(stat="identity", aes(fill=Mission_Class)) + coord_flip() + labs(y="Flight Hours") + ggtitle("Flight Hours by Tail Number, Achieved or Missed"))
# pareto by missed hours, then by achieved hours
allFlightHrsByTNwide <- group_by(flightHrsByTN, Serial_Number, flightType) %>% 
  summarise(allFlightHours = sum(flightHours, na.rm=TRUE)) %>% spread(flightType,allFlightHours,fill=0)
missedHourSNorder <- as.character(allFlightHrsByTNwide$Serial_Number[order(allFlightHrsByTNwide$missedHours,allFlightHrsByTNwide$achievedHours)])
#allMissedHoursByTN <- group_by(missedHoursByTN, Serial_Number) %>% summarise(allMissedHours = sum(missedHours))
#missedHourSNorder <- as.character(allMissedHoursByTN$Serial_Number[order(allMissedHoursByTN$allMissedHours)]) # level order
flightHrsByTN <- within(flightHrsByTN, Serial_Number <- factor(Serial_Number, levels = missedHourSNorder))
(gtn <- ggplot(flightHrsByTN, aes(x=Serial_Number,y=flightHours)) + geom_bar(stat="identity", aes(fill=flightType)) + 
  coord_flip() + labs(y="Flight Hours", x="Serial Number") + ggtitle("Flight Hours by Tail Number, Achieved or Missed after Abort") +
  scale_fill_manual(labels=c("Achieved Hours","Missed Hours b/c Abort"),name="Hours Type",
                    values=c(rgb(3/365,132/365,173/365),"firebrick2")) + 
  theme(title=element_text(size=22),legend.title=element_text(size=20),legend.text=element_text(size=16),
        axis.text=element_text(size=12),axis.title=element_text(size=18)))
ggsave(filename="tail_number_flight_hours_byMissedAchieved_sortByMissed.svg", plot=gtn, width=18, height=12, scale=1)
### maybe ADD IN SERIAL NUMBERS WITH NO FLIGHTS

## missed and achieved hours by debrief week:
weekStarts <- unique(sortieHrs[sortieHrs$Sortie_DayOfWeek %in% "Sun","Sortie_Date"])
weekEnds <- unique(sortieHrs[sortieHrs$Sortie_DayOfWeek %in% "Sat","Sortie_Date"])
#debriefWeeks <- data.frame("weekStarts"=sort(weekStarts$Sortie_Date), "weekEnds"=c(sort(weekEnds$Sortie_Date),NA))
debriefWeeks <- data.frame("weekStarts"=sort(weekStarts$Sortie_Date)[seq(nrow(weekStarts)-1)], "weekEnds"=sort(weekEnds$Sortie_Date))
debriefWeeks$intervalWeek <- interval(debriefWeeks$weekStarts,debriefWeeks$weekEnds)
# add week info to flightHrsByTN
flightHrsByTN$Sortie_WeekStart <-  as.character(max(weekStarts$Sortie_Date)) # the final week is only a partial week
for(ii in seq(nrow(debriefWeeks))) {
  flightHrsByTN$Sortie_WeekStart <- ifelse(flightHrsByTN$Sortie_Date %within% debriefWeeks$intervalWeek[ii], as.character(debriefWeeks$weekStarts[ii]), flightHrsByTN$Sortie_WeekStart)
}
flightHrsByTN$Sortie_WeekStart <- ymd(flightHrsByTN$Sortie_WeekStart) # not sure why I have to convert to character and back to date
flightHrsByTN <- flightHrsByTN[!is.na(flightHrsByTN$Sortie_WeekStart),] # drop the NAs
(gtn <- ggplot(flightHrsByTN, aes(x=Serial_Number,y=flightHours)) + geom_bar(stat="identity", aes(fill=flightType)) + coord_flip() + labs(y="Flight Hours") + facet_grid(.~Sortie_WeekStart) + ggtitle("Flight Hours by Tail Number, Achieved or Missed by Week"))
ggsave(filename="tail_number_flight_hours_byMissedAchieved_byWeek.svg", plot=gtn, width=20, height=12, scale=1)

#### tail number lifetime
achievedHrsByTN <- achievedHrsByTN[order(achievedHrsByTN$Serial_Number, achievedHrsByTN$Sortie_Date),]
achievedHrsByTNaggHr <- group_by(achievedHrsByTN,Serial_Number,Sortie_Date) %>% summarise(dayAchievedHours = sum(achievedHours)) %>% mutate(acrFlHr = cumsum(dayAchievedHours))
(gps <- ggplot(achievedHrsByTNaggHr, aes(x=Sortie_Date,y=acrFlHr)) + geom_line() + facet_wrap(~Serial_Number) + ggtitle("Accrued Flight Hours by Tail Number"))
ggsave(filename="tail_number_accrued_flight_hours_byDate_grid.svg", plot=gps, width=20, height=12, scale=1)
(gps <- ggplot(achievedHrsByTNaggHr, aes(x=Sortie_Date,y=acrFlHr, colour=Serial_Number)) + geom_line(size=1) + ggtitle("Accrued Flight Hours by Tail Number"))
ggsave(filename="tail_number_accrued_flight_hours_byDate_onePlot.svg", plot=gps, width=20, height=12, scale=1)

# all days 
achievedHrsByTNaggHr$Sortie_Date <- ymd(achievedHrsByTNaggHr$Sortie_Date)
lifetimes <- expand.grid("Serial_Number"=unique(achievedHrsByTNaggHr$Serial_Number),"Sortie_Date"=ymd((as.Date("2014-05-31 UTC")+seq(61))))
lifetimes <- right_join(achievedHrsByTNaggHr,lifetimes,by=c("Serial_Number","Sortie_Date"))
lifetimes$acrFlHr <- NULL # redo this with zeros
lifetimes[is.na(lifetimes$dayAchievedHours),]$dayAchievedHours <- 0 # set to zero
lifetimes <- group_by(lifetimes, Serial_Number) %>% mutate(acrFlHr = cumsum(dayAchievedHours))
(gps <- ggplot(lifetimes, aes(x=Sortie_Date,y=acrFlHr, colour=Serial_Number)) + geom_line(size=1) + ggtitle("Accrued Flight Hours by Tail Number"))
# bring in events - Remove for Cann / 
eventsDB <- select(oem, Serial_Number, Transaction_Date, Action_Taken_Code, Work_Unit_Code, WUC_Narrative) %>% filter(Action_Taken_Code %in% c("U","T")) %>% distinct()
# relevel all factors
eventsDB[,lapply(eventsDB,class)=="factor"] <- lapply(eventsDB[,lapply(eventsDB,class)=="factor"],factor)
# trim to June/July
eventsDB <- eventsDB[eventsDB$Transaction_Date > '2014-05-31' & eventsDB$Transaction_Date < '2014-08-01',]
# merge with flight hours data, but first add to flight hours the SNs that are in maintenance but do not fly
maintTNlives <- expand.grid("Serial_Number"=unique(eventsDB[!(eventsDB$Serial_Number %in% lifetimes$Serial_Number),]$Serial_Number),"Sortie_Date"=ymd((as.Date("2014-05-31 UTC")+seq(61))))
maintTNlives$dayAchievedHours <- 0; maintTNlives$acrFlHr <- 0
lifetimes <- rbind(lifetimes,maintTNlives) # join to previous flight hours db
# rename fields
names(lifetimes) <- c("Serial_Number", "Event_Date", names(lifetimes)[3:4])
names(eventsDB) <- c("Serial_Number", "Event_Date", names(eventsDB)[3:5])
# now combine both flight and events DB
lifetimesWevent <- right_join(eventsDB, select(lifetimes, Serial_Number, Event_Date, acrFlHr), by=c("Serial_Number","Event_Date"))
( gglife <- ggplot(lifetimesWevent, aes(x=Event_Date,y=acrFlHr,colour=Serial_Number)) + 
  geom_line() + geom_point(data=lifetimesWevent[!is.na(lifetimesWevent$Action_Taken_Code),],aes(x=Event_Date,y=acrFlHr,shape=Action_Taken_Code),cex=4) +
  scale_shape_manual(name="Action Taken",labels=c("T:Cann Removal","U:Cann Install"),values=c(16,17)))
ggsave(filename="tail_number_lifetime_cannRems_fullFleet.svg", plot=gglife, width=20, height=12, scale=1)
( gglifePy <- plot_ly(lifetimesWevent, x=Event_Date,y=acrFlHr, color=Serial_Number) %>%
  layout(title="Achieved Hours and Cannibalizations by Serial Number",
         xaxis=list(title="Date"),yaxis=list(title="Achieved Flight Hours")) %>%
  add_trace(data=lifetimesWevent[lifetimesWevent$Action_Taken_Code %in% "U",],x=Event_Date,y=acrFlHr,
            opacity=0.6,name="Cannibalization Installs",hoverinfo="x+y+text",
            mode="markers",marker=list(color="blue", size=10),text=paste0("SN: ",Serial_Number,"<br>Work Unit Code: ",Work_Unit_Code,"<br>",WUC_Narrative)) %>%
  add_trace(data=lifetimesWevent[lifetimesWevent$Action_Taken_Code %in% "T",],x=Event_Date,y=acrFlHr,
            opacity=0.6,name="Cannibalization Removals",hoverinfo="x+y+text",
            mode="markers",marker=list(color="red", size=11),text=paste0("SN: ",Serial_Number,"<br>Work Unit Code: ",Work_Unit_Code,"<br>",WUC_Narrative)))

# to test labels for removals
# a <- lifetimesWevent[round(runif(25,1,500)),];a[,lapply(a,class)=="factor"] <- lapply(a[,lapply(a,class)=="factor"],factor)
# ( ggplot(a, aes(x=Event_Date,y=acrFlHr,colour=Serial_Number)) + 
#   geom_line() + geom_point(data=a[!is.na(a$Action_Taken_Code),],aes(x=Event_Date,y=acrFlHr,shape=Action_Taken_Code),cex=4) + 
#   scale_shape_manual(name="Action Taken",labels=c("T:Cann Removal","U:Cann Install"),values=c(3,4)) )

########### gg plot adds values from rows with same factor - however, the order
abc <- data.frame("c1" = c("a","a","b","b"), "c2"=c(1,2,3,1), "c3"=c("c","d","c","d"))
(ggplot(abc, aes(c1,c2)) + geom_bar(stat="identity", aes(fill=c3)))

################# subsystem wuc paretos - debrief
debriefSubWUcs <- group_by(debrief, Sortie_Number, Sortie_Date, Subsystem_Work_Unit_Code, Subsystem_WUC_Description)
# group by sortie and subystem-wuc
debriefSubWUcs <- summarise(debriefSubWUcs, count=n())
# leave out NAs (description not found), which show up in 48% of subsystem wuc descriptions (likely other subsystems in those sorties as well)
debriefSubWUcs <- debriefSubWUcs[!is.na(debriefSubWUcs$Subsystem_Work_Unit_Code),]
# re-level
debriefSubWUcs$Subsystem_WUC_Description <- factor(debriefSubWUcs$Subsystem_WUC_Description)
debriefSubWUcs$Subsystem_Work_Unit_Code <- factor(debriefSubWUcs$Subsystem_Work_Unit_Code)
# count sorties with a subsystem wuc (some may show up twice)
debriefSubWUcsShowUpTwice <- group_by(debriefSubWUcs, Sortie_Number, Sortie_Date)
debriefSubWUcsShowUpTwice <- summarise(debriefSubWUcsShowUpTwice, count=n()) # 16 show up twice
# first get the order of the levels
debriefSubWUCsorder <- names(sort(table(debriefSubWUcs$Subsystem_WUC_Description),decreasing=TRUE))
# count sorties per subsystem wuc
debriefSubWUcs <- group_by(debriefSubWUcs, Subsystem_Work_Unit_Code, Subsystem_WUC_Description)
debriefSubWUcs <- summarise(debriefSubWUcs, count = n())
# now plot a pareto
debriefSubWUcs <- within(debriefSubWUcs, Subsystem_WUC_Description <- factor(
  Subsystem_WUC_Description, levels = rev(debriefSubWUCsorder)))
gb <- ggplot(debriefSubWUcs, aes(x=Subsystem_WUC_Description,y=count)) +
  geom_bar(stat="identity") + labs(y="Sorties Subsystem Appears In, Debrief Data") + coord_flip()
gb
ggsave(filename="subsystem_wuc_sorties_pareto_debrief.svg", plot=gb, scale=3)

# try only ground aborts
debriefSubWUcsGA <- group_by(debrief, Sortie_Number, Sortie_Date, Subsystem_Work_Unit_Code, Subsystem_WUC_Description) %>% filter(Deviation_Code %in% 'GA') %>% summarise(count=n())
debriefWUcsGA <- group_by(debrief, Sortie_Number, Sortie_Date, Work_Unit_Code, WUC_Description) %>% filter(Deviation_Code %in% 'GA') %>% summarise(count=n())
#relevel
debriefSubWUcsGA$Subsystem_Work_Unit_Code <- factor(debriefSubWUcsGA$Subsystem_Work_Unit_Code);debriefSubWUcsGA$Subsystem_WUC_Description <- factor(debriefSubWUcsGA$Subsystem_WUC_Description)
debriefWUcsGA$WUC_Description <- factor(debriefWUcsGA$WUC_Description); debriefWUcsGA$Work_Unit_Code <- factor(debriefWUcsGA$Work_Unit_Code)

#########################################################################################################
## On Equipment Maintenance Data
oem <- sqlQuery(udri, "SELECT * FROM on_equipment_maintenance")
# adjust column types
#oem$On_Work_Order_Key <- as.factor(oem$On_Work_Order_Key) 
#oem$On_Maint_Action_Key <- as.factor(oem$On_Maint_Action_Key) 
oem$Serial_Number <- as.factor(oem$Serial_Number)
oem$Record_Identifier <- as.factor(oem$Record_Identifier)
oem$Discrepancy_Narrative <- as.character(oem$Discrepancy_Narrative)
oem$Corrective_Narrative <- as.character(oem$Corrective_Narrative)
oem$How_Malfunction_Code <- as.factor(oem$How_Malfunction_Code)
oem$How_Malfunction_Class_Ind <- as.factor(oem$How_Malfunction_Class_Ind)
oem$AFTO_Form_350_Tag_Number <- as.factor(oem$AFTO_Form_350_Tag_Number)
oem$Work_Center_Event_Identifier <- as.factor(oem$Work_Center_Event_Identifier)
# add date information
oem$Transaction_Date <- ymd(oem$Transaction_Date, tz='UTC')
oem$Transaction_DayOfWeek <- wday(oem$Transaction_Date, label = TRUE)
oem$Transaction_Month <- as.factor(month(oem$Transaction_Date))
oem$Transaction_DayOfMonth <- day(oem$Transaction_Date)
# turn JCN into a factor that matches debrief data (take out the ".0" at the end)
oem$Job_Control_Number <- as.character(oem$Job_Control_Number)
oem$Job_Control_Number <- gsub("\\.0","",oem$Job_Control_Number)
oem$Job_Control_Number <- factor(oem$Job_Control_Number)
# turn WUC into a factor that matches debrief data (take out the ".0" at the end)
oem$Work_Unit_Code <- as.character(oem$Work_Unit_Code)
oem$Work_Unit_Code <- gsub("\\.0","",oem$Work_Unit_Code)
# fix a few weird ones
#a<-levels(oem$Work_Unit_Code[grep("^0[1-9][0-9A-Za-z]{3}$",levels(oem$Work_Unit_Code))])
oem$Work_Unit_Code[grep("e",oem$Work_Unit_Code)] <- gsub("\\.","",oem$Work_Unit_Code[grep("e",oem$Work_Unit_Code)]) # scientific notation
oem$Work_Unit_Code[grep("e",oem$Work_Unit_Code)] <- gsub("e\\+100","E99",oem$Work_Unit_Code[grep("e",oem$Work_Unit_Code)])
# still some WUCs that are 2 characters and 10 characters - I'll leave these b/c I dont' know what to do w/ them
# add a zero to the 4-digit wucs
oem$Work_Unit_Code[grep("^[A-Z0-9]{4}$",oem$Work_Unit_Code)] <- paste0("0",oem$Work_Unit_Code[grep("^[A-Z0-9]{4}$",oem$Work_Unit_Code)])
oem$Work_Unit_Code <- factor(oem$Work_Unit_Code)
#################### / LOAD & TRANSFORM DATA END

# date histogram
qplot(Transaction_Date, data = oem, fill=Geographic_Location)
qplot(Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01',], fill=Geographic_Location)

# weird fields
oem[which(oem$On_Maint_Action_Key==max(oem$On_Maint_Action_Key)),"Transaction_Date"]
qplot(y=oem$On_Work_Order_Key, x=oem$Transaction_Date)
qplot(y=On_Work_Order_Key, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01' & oem$Geographic_Location == 'FXBM',])
# on maintenance action key
qplot(y=oem$On_Maint_Action_Key, x=oem$Transaction_Date)
qplot(y=On_Maint_Action_Key, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01',], colour=Transaction_Month)
qplot(y=On_Maint_Action_Key, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01',], colour=Geographic_Location)
qplot(y=On_Maint_Action_Key, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01' & oem$Geographic_Location == 'FXBM',])
qplot(y=On_Maint_Action_Key, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01' & oem$Geographic_Location == 'FXBM' & oem$On_Maint_Action_Key < 100,])
g<-ggplot(oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01' & oem$Geographic_Location == 'FXBM' & oem$On_Maint_Action_Key < 100,], aes(Transaction_Date, On_Maint_Action_Key)) + stat_binhex(bins=60); print(g)
g<-ggplot(oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01' ,], aes(Transaction_Date, On_Maint_Action_Key)) + stat_binhex(bins=60); print(g)
# sequence number
qplot(y=Sequence_Number, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01',], colour=Geographic_Location)
qplot(y=Sequence_Number, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01',], colour=Transaction_Month)
qplot(y=Sequence_Number, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01' & oem$Geographic_Location == 'FNWZ',], colour=Transaction_Month)
qplot(y=Sequence_Number, x=Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01' & oem$Geographic_Location == 'FNWZ',], colour=Transaction_Month)

# current operating time, or time since overhaul (according to data dictionary 4.11)
qplot(oem$Current_Operating_Time)
qplot(oem[oem$Current_Operating_Time>0,]$Current_Operating_Time)
qplot(oem[oem$Install_Prev_Operating_Time>0,]$Install_Prev_Operating_Time); (length(oem[oem$Install_Prev_Operating_Time>0,]$Install_Prev_Operating_Time)/sum(!is.na(oem$Install_Prev_Operating_Time)))
qplot(oem[oem$Install_Current_Operating_Time>0,]$Install_Current_Operating_Time); (length(oem[oem$Install_Current_Operating_Time>0,]$Install_Current_Operating_Time)/sum(!is.na(oem$Install_Current_Operating_Time)))

# text
# try some advanced stuff
# discrepancy narrative
oemDiscrepancyVector <- VectorSource(oem$Discrepancy_Narrative) # convert to vector of words
oemDiscrepancyCorpus <- Corpus(oemDiscrepancyVector) # convert to corpus
#str(documentCorpus)
oemDiscrepancyCorpus <- tm_map(oemDiscrepancyCorpus, removeWords, stopwords("english")) # remove stop words
# das - data analysis system
wordcloud(oemDiscrepancyCorpus,max.words=100)
# event narrative
oemWorkCenterEventNarrative <- VectorSource(oem$Work_Center_Event_Narrative) # convert to vector of words
oemWorkCenterEventNarrativeCorpus <- Corpus(oemWorkCenterEventNarrative) # convert to corpus
#str(documentCorpus)
oemWorkCenterEventNarrativeCorpus <- tm_map(oemWorkCenterEventNarrativeCorpus, removeWords, stopwords("english")) # remove stop words
wordcloud(oemWorkCenterEventNarrativeCorpus,max.words=100)


## maintenance times
jcnTimeSpans <- group_by(oem,Job_Control_Number)
jcnTimeSpans <- summarise(jcnTimeSpans, maxDate = max(Transaction_Date), minDate = min(Transaction_Date))
jcnTimeSpans$daysSpan <- jcnTimeSpans$maxDate - jcnTimeSpans$minDate
qplot(as.numeric(jcnTimeSpans$daysSpan),xlim=c(0,15))
## WUC pareto
# By Labor Hours, Occurances, and Days
# filter out nondescript maintenance (cases with no PN) 
# filter(!is.na(On_Component_Part_Number)) previous code used this filter, now I split into Support General and not
# all JCN
oemWUCDescpareto <- group_by(oem, Work_Unit_Code,WUC_Narrative, Job_Control_Number) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours), minDt = min(Transaction_Date), maxDt = max(Transaction_Date))
oemWUCDescpareto$maintDays <- 1+difftime(oemWUCDescpareto$maxDt,oemWUCDescpareto$minDt,units="days") # add days, minimum 1 per JCN (includes JCN overlaps)
# aggregate over JCN
oemWUCDescpareto <-group_by(oemWUCDescpareto,Work_Unit_Code,WUC_Narrative) %>% summarise(JCNs=n(),totalMaintDays = sum(maintDays),count=sum(count),lbrHrs=sum(lbrHrs))
#oemWUCDescpareto <- group_by(oem, Work_Unit_Code,WUC_Narrative) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours))
# classify as Special Purpose WUCs instead of null part numbers
# specialPurposeWUCs <- c(01000,02000,03000,03100,03101,03102,03107,03111,03112,03113,03114,03115,'0311K','0311L','0311M','0311N','0311P','0311R','0311S','0311T','0311U',03121,03128,03130,03142,03156,03184,03200,03205,03209,03210,03212,03215,03220,03221,03268,03300,03305,03310,03311,03312,03313,03314,03320,03330,03336,03340,03360,03370,03380,03390,03395,03400,'0341A','0341B','0341C','0341D','0341E','0341F','0341G','0341H','0341J','0341K','0341L','0341M','0341N','0341P','0341Q','0341R','0341S','0341T','0341U','0341V','0341W','0341X','0341Y','0341Z','0342A','0342B',03510,03580,03596,03597,03600,03610,03700,03710,03711,03712,03713,03714,03720,03721,03722,03723,03724,03730,03731,03732,03750,03755,03800,03802,03803,03804,03806,03900,03999,04000,04100,04101,04110,04111,04112,04113,04114,04115,04116,04117,04118,04119,'0411A','0411B','0411C','0411D','0411E','0411H','0411J','0411K',04120,04121,04122,04123,04124,04125,04126,04127,04128,04129,'0412A','0412B','0412C','0412D','0412E','0412F','0412G','0412H','0412J','0412H','0412J','0412L','0412M','0412N','0412P','0412Q',04130,04131,04132,04133,04134,04135,04136,04137,04138,04139,'0413A','0413B','0413C','0413E','0413F','0413H','0413J','0413K','0413L','0413M','0413N','0413P',04140,04141,04142,04143,04144,04145,04146,04147,04148,04149,04150,04151,04152,'0415A','0415B','0415C',04160,04161,04162,04163,04170,04180,04181,04182,04184,04185,04186,04187,04188,04189,'0418A','0418B','0418C','0418D','0418E','0418F',04190,04199,04200,04210,04220,04221,04222,04227,04228,04270,04280,04310,04311,04313,04314,04315,04316,04317,04320,04321,04322,04324,04325,04326,04327,04330,04340,04341,04342,04343,04344,04345,04346,04347,04348,04349,04350,04351,04352,04353,04354,04355,04356,04358,04359,04360,04361,04362,04363,04364,04365,04366,04367,04370,04371,04372,04373,04400,04500,04510,04572,04573,04574,04575,04576,04577,04578,04583,04584,04610,04620,04630,04650,04660,04999,'04MD4',05000,06000,07000,08000,09000)
# supportGeneralWUCs <- c('1000.0','1100.0','2000.0','3000.0','3100.0','3101.0','3102.0','3107.0','3111.0','3112.0','3113.0','3114.0','3115.0', '0311K', '0311L', '0311M', '0311N', '0311P', '0311R', '0311S', '0311T', '0311U','3121.0','3128.0','3130.0','3142.0','3156.0','3184.0','3200.0','3205.0','3209.0','3210.0','3212.0','3215.0','3220.0','3221.0','3268.0','3300.0','3305.0','3310.0','3311.0','3312.0','3313.0','3314.0','3320.0','3330.0','3336.0','3340.0','3360.0','3370.0','3380.0','3390.0','3395.0','3400.0', '0341A', '0341B', '0341C', '0341D', '0341E', '0341F', '0341G', '0341H', '0341J', '0341K', '0341L', '0341M', '0341N', '0341P', '0341Q', '0341R', '0341S', '0341T', '0341U', '0341V', '0341W', '0341X', '0341Y', '0341Z', '0342A', '0342B','3510.0','3580.0','3596.0','3597.0','3600.0','3610.0','3700.0','3710.0','3711.0','3712.0','3713.0','3714.0','3720.0','3721.0','3722.0','3723.0','3724.0','3730.0','3731.0','3732.0','3750.0','3755.0','3800.0','3802.0','3803.0','3804.0','3806.0','3900.0','3999.0','4000.0','4100.0','4101.0','4110.0','4111.0','4112.0','4113.0','4114.0','4115.0','4116.0','4117.0','4118.0','4119.0', '0411A', '0411B', '0411C', '0411D', '0411E', '0411H', '0411J', '0411K','4120.0','4121.0','4122.0','4123.0','4124.0','4125.0','4126.0','4127.0','4128.0','4129.0', '0412A', '0412B', '0412C', '0412D', '0412E', '0412F', '0412G', '0412H', '0412J', '0412H', '0412J', '0412L', '0412M', '0412N', '0412P', '0412Q','4130.0','4131.0','4132.0','4133.0','4134.0','4135.0','4136.0','4137.0','4138.0','4139.0', '0413A', '0413B', '0413C', '0413E', '0413F', '0413H', '0413J', '0413K', '0413L', '0413M', '0413N', '0413P','4140.0','4141.0','4142.0','4143.0','4144.0','4145.0','4146.0','4147.0','4148.0','4149.0','4150.0','4151.0','4152.0', '0415A', '0415B', '0415C','4160.0','4161.0','4162.0','4163.0','4170.0','4180.0','4181.0','4182.0','4184.0','4185.0','4186.0','4187.0','4188.0','4189.0', '0418A', '0418B', '0418C', '0418D', '0418E', '0418F','4190.0','4199.0','4200.0','4210.0','4220.0','4221.0','4222.0','4227.0','4228.0','4270.0','4280.0','4310.0','4311.0','4313.0','4314.0','4315.0','4316.0','4317.0','4320.0','4321.0','4322.0','4324.0','4325.0','4326.0','4327.0','4330.0','4340.0','4341.0','4342.0','4343.0','4344.0','4345.0','4346.0','4347.0','4348.0','4349.0','4350.0','4351.0','4352.0','4353.0','4354.0','4355.0','4356.0','4358.0','4359.0','4360.0','4361.0','4362.0','4363.0','4364.0','4365.0','4366.0','4367.0','4370.0','4371.0','4372.0','4373.0','4400.0','4500.0','4510.0','4572.0','4573.0','4574.0','4575.0','4576.0','4577.0','4578.0','4583.0','4584.0','4610.0','4620.0','4630.0','4650.0','4660.0','4999.0', '04MD4','5000.0','6000.0','7000.0','8000.0','9000.0')
wucs <- levels(oem$Work_Unit_Code)
gsWucs<-wucs[grep("^0[1-9][0-9A-Za-z]{3}$",wucs)]
# add WUC Type
oemWUCDescpareto$WUC_Type[oemWUCDescpareto$Work_Unit_Code %in% gsWucs] <- "Support General"
oemWUCDescpareto$WUC_Type[!(oemWUCDescpareto$Work_Unit_Code %in% gsWucs)] <- "Bill of Material"
oemWUCDescpareto$WUC_Type[is.na(oemWUCDescpareto$Work_Unit_Code)] <- "None"
oemWUCDescpareto$WUC_Type <- as.factor(oemWUCDescpareto$WUC_Type)
# Add missing Work_Unit_Code and WUC_Narrative
oemWUCDescpareto$Work_Unit_Code<-as.character(oemWUCDescpareto$Work_Unit_Code);oemWUCDescpareto$WUC_Narrative<-as.character(oemWUCDescpareto$WUC_Narrative)
oemWUCDescpareto$Work_Unit_Code[is.na(oemWUCDescpareto$Work_Unit_Code)]<-"None";oemWUCDescpareto$WUC_Narrative[is.na(oemWUCDescpareto$WUC_Narrative)]<-"No Work Unit Code"
oemWUCDescpareto$Work_Unit_Code<-factor(oemWUCDescpareto$Work_Unit_Code);oemWUCDescpareto$WUC_Narrative<-factor(oemWUCDescpareto$WUC_Narrative)
(gWucCnLb <- ggplot(oemWUCDescpareto, aes(x=count,y=lbrHrs,colour=WUC_Type)) + labs(x="Maintenance Actions",y="Labor Manhours",title="Work Unit Code Comparison") +
  geom_point(aes(text=paste("WUC:",Work_Unit_Code,"Narrative:",WUC_Narrative))))
ggsave(filename="wuc_scatter.svg", plot=gWucCnLb, width=15, height=9,scale=1)
(gWucCnLb_plotly <- plot_ly(oemWUCDescpareto, x=count, y=lbrHrs, color=WUC_Type, mode="markers", text=paste0(Work_Unit_Code,"<br>",WUC_Narrative)) %>%
  layout(title="Work Unit Code Comparison, Overall Data", xaxis=list(title="Maintenance Actions"), yaxis=list(title="Labor Manhours")))

##### focus on all records in OEM first
# relevel
oemWUCDescpareto$WUC_Narrative <- factor(oemWUCDescpareto$WUC_Narrative)
# 1) labor hours
# get the order of the levels and re-order
oemWUCDescparetoOrder <- as.character(oemWUCDescpareto[order(-oemWUCDescpareto$lbrHrs),"Work_Unit_Code"]$Work_Unit_Code)
oemWUCDescpareto <- within(oemWUCDescpareto, Work_Unit_Code <- factor(
  Work_Unit_Code, levels = rev(oemWUCDescparetoOrder)))
# now plot pareto - top 50
oemWUCDescparetoTop50 <- oemWUCDescpareto[oemWUCDescpareto$Work_Unit_Code %in% oemWUCDescparetoOrder[1:50],]
oemWUCDescparetoTop50 <- within(oemWUCDescparetoTop50, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(oemWUCDescparetoOrder,50))))
(gb <- ggplot(oemWUCDescparetoTop50, aes(x=Work_Unit_Code,y=lbrHrs,fill=WUC_Type)) +
  geom_bar(stat="identity") + labs(x="Work Unit Code",y="Total Labor Hours, OEM Data",title="Top 50 Work Unit Codes by Labor Hours") + 
  coord_flip() + scale_fill_manual(values=c("lightblue","grey79","palegreen2")) +
  geom_text(cex=4,aes(hjust=0,x=Work_Unit_Code,y=0,label=paste(WUC_Narrative))) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) ) # how to change the labels?
ggsave(filename="wuc_labor_hours_pareto_oem_top50.svg", plot=gb, width=15, height=9, scale=1)
# top 20 for plotly
oemWUCDescparetoTop20 <- oemWUCDescpareto[oemWUCDescpareto$Work_Unit_Code %in% oemWUCDescparetoOrder[1:20],]
oemWUCDescparetoTop20 <- within(oemWUCDescparetoTop20, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(oemWUCDescparetoOrder,20))))
oemWUCDescparetoTop20 <- oemWUCDescparetoTop20[order(oemWUCDescparetoTop20$lbrHrs,decreasing = FALSE),]
oemWUCDescparetoTop20$Work_Unit_Code <- as.character(oemWUCDescparetoTop20$Work_Unit_Code)
oemWUCDescparetoTop20$WUC_Narrative <- as.character(oemWUCDescparetoTop20$WUC_Narrative)
(gbpy <- plot_ly(oemWUCDescparetoTop20, y=Work_Unit_Code, x=lbrHrs, orientation="h", 
                  group=WUC_Type,type="bar",text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",WUC_Narrative)) %>%
   layout(yaxis=list(type="category",title="Work Unit Code"),xaxis=list(title="Total Labor Hours"),
          margin=list("l"=160,"r"=0,"t"=0,"b"=60)) )
# hack it to order by all WUCs, not within group
(gbpy <- plot_ly(oemWUCDescparetoTop20, y=Work_Unit_Code, x=lbrHrs, orientation="h", 
                 group=WUC_Type,type="bar",text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",WUC_Narrative)) %>%
  layout(yaxis=list(type="category",title="Work Unit Code"),xaxis=list(title="Total Labor Hours"),
         margin=list("l"=160,"r"=0,"t"=0,"b"=60)) )
oemWUCDescparetoTop20 <- select(oemWUCDescparetoTop20,Work_Unit_Code,WUC_Narrative,WUC_Type,lbrHrs)
oemWUCDescparetoTop20 <- spread(oemWUCDescparetoTop20,WUC_Type,lbrHrs,fill=0)
names(oemWUCDescparetoTop20) <- c(names(oemWUCDescparetoTop20)[1:2],"Bill_of_Material","Support_General")
oemWUCDescparetoTop20 <- oemWUCDescparetoTop20[order(oemWUCDescparetoTop20$Bill_of_Material+oemWUCDescparetoTop20$Support_General),]
(gbpy <- plot_ly(oemWUCDescparetoTop20, y=Work_Unit_Code, x=Bill_of_Material, orientation="h", marker=list(color="steelblue"),
                 name="Bill of Material",type="bar",oemWUCDescparetoTop20type="bar",text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",WUC_Narrative)) %>%
  add_trace(oemWUCDescparetoTop20, y=Work_Unit_Code, x=Support_General, orientation="h", marker=list(color="olivedrab"),
            name="Support General",type="bar",text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",WUC_Narrative)) %>%
  layout(barmode="stack",yaxis=list(type="category",title="Work Unit Code"),xaxis=list(title="Total Labor Hours"),
         margin=list("l"=160,"r"=0,"t"=0,"b"=60)) )

plotly_POST(gbpy,filename="wucPareto1",world_readable=FALSE,fileopt="overwrite") # saved as plotly 101


# merge WUCs into length-(2,3,4) WUCs
oemWUCDescpareto$Work_Unit_Code_G2 <- substr(as.character(oemWUCDescpareto$Work_Unit_Code),1,2)
oemWUCDescpareto$Work_Unit_Code_G3 <- substr(as.character(oemWUCDescpareto$Work_Unit_Code),1,3)
oemWUCDescpareto$Work_Unit_Code_G4 <- substr(as.character(oemWUCDescpareto$Work_Unit_Code),1,4)
# fix none
oemWUCDescpareto[oemWUCDescpareto$Work_Unit_Code %in% "None",c("Work_Unit_Code_G2","Work_Unit_Code_G3","Work_Unit_Code_G4")] <- "None"
# get order
oemWUCDescpareto2 <- group_by(oemWUCDescpareto,Work_Unit_Code_G2,WUC_Type) %>% summarise(JCNs = sum(JCNs),count=sum(count),lbrHrs=sum(lbrHrs),totalMaintDays=sum(totalMaintDays))
oemWUCDescpareto3 <- group_by(oemWUCDescpareto,Work_Unit_Code_G3,WUC_Type) %>% summarise(JCNs = sum(JCNs),count=sum(count),lbrHrs=sum(lbrHrs),totalMaintDays=sum(totalMaintDays))
# add subsystem description

oemWUCDescpareto4 <- group_by(oemWUCDescpareto,Work_Unit_Code_G4,WUC_Type) %>% summarise(JCNs = sum(JCNs),count=sum(count),lbrHrs=sum(lbrHrs),totalMaintDays=sum(totalMaintDays))
oemWUCDescparetoOrder2 <- as.character(oemWUCDescpareto2[order(-oemWUCDescpareto2$lbrHrs),"Work_Unit_Code_G2"]$Work_Unit_Code_G2)
oemWUCDescparetoOrder3 <- as.character(oemWUCDescpareto3[order(-oemWUCDescpareto3$lbrHrs),"Work_Unit_Code_G3"]$Work_Unit_Code_G3)
oemWUCDescparetoOrder4 <- as.character(oemWUCDescpareto4[order(-oemWUCDescpareto4$lbrHrs),"Work_Unit_Code_G4"]$Work_Unit_Code_G4)
oemWUCDescpareto2 <- within(oemWUCDescpareto2, Work_Unit_Code_G2 <- factor(Work_Unit_Code_G2, levels = rev(oemWUCDescparetoOrder2)))
(gb <- ggplot(oemWUCDescpareto2, aes(x=Work_Unit_Code_G2,y=lbrHrs,fill=WUC_Type)) +
  geom_bar(stat="identity") + labs(x="Work Unit Code Group",y="Total Labor Hours, OEM Data",title="Ranked Work Unit Code Groups - 2 Characters - by Labor Hours") + 
  coord_flip() + scale_fill_manual(values=c("lightblue","grey79","palegreen2")) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) ) # how to change the labels?
ggsave(filename="wucGroup2_labor_hours_pareto_oem.svg", plot=gb, width=15, height=9, scale=1)
(gWucCnLbG2_plotly <- plot_ly(oemWUCDescpareto2, x=count, y=lbrHrs, group=WUC_Type, mode="markers", text=paste0(Work_Unit_Code_G2)) %>%
  layout(title="Work Unit Code Comparison, Overall Data", xaxis=list(title="Maintenance Actions"), yaxis=list(title="Labor Manhours")))
oemWUCDescpareto3 <- oemWUCDescpareto3[oemWUCDescpareto3$Work_Unit_Code_G3 %in% oemWUCDescparetoOrder3[1:50],]
oemWUCDescpareto3 <- within(oemWUCDescpareto3, Work_Unit_Code_G3 <- factor(Work_Unit_Code_G3, levels = rev(head(oemWUCDescparetoOrder3,50))))
(gb <- ggplot(oemWUCDescpareto3, aes(x=Work_Unit_Code_G3,y=lbrHrs,fill=WUC_Type)) +
  geom_bar(stat="identity") + labs(x="Work Unit Code Group",y="Total Labor Hours, OEM Data",title="Ranked Work Unit Code Groups - 3 Characters - by Labor Hours") + 
  coord_flip() + scale_fill_manual(values=c("lightblue","grey79","palegreen2")) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) ) # how to change the labels?
ggsave(filename="wucGroup3_labor_hours_pareto_oem.svg", plot=gb, width=15, height=9, scale=1)
(gWucCnLbG3_plotly <- plot_ly(oemWUCDescpareto3, x=count, y=lbrHrs, group=WUC_Type,mode="markers", text=paste0(Work_Unit_Code_G3)) %>%
  layout(title="Work Unit Code Comparison, Overall Data", xaxis=list(title="Maintenance Actions"), yaxis=list(title="Labor Manhours")))
oemWUCDescpareto4 <- oemWUCDescpareto4[oemWUCDescpareto4$Work_Unit_Code_G4 %in% oemWUCDescparetoOrder4[1:50],]
oemWUCDescpareto4 <- within(oemWUCDescpareto4, Work_Unit_Code_G4 <- factor(Work_Unit_Code_G4, levels = rev(head(oemWUCDescparetoOrder4,50))))
(gb <- ggplot(oemWUCDescpareto4, aes(x=Work_Unit_Code_G4,y=lbrHrs,fill=WUC_Type)) +
  geom_bar(stat="identity") + labs(x="Work Unit Code Group",y="Total Labor Hours, OEM Data",title="Ranked Work Unit Code Groups - 4 Characters - by Labor Hours") + 
  coord_flip() + scale_fill_manual(values=c("lightblue","grey79","palegreen2")) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) ) # how to change the labels?
ggsave(filename="wucGroup4_labor_hours_pareto_oem.svg", plot=gb, width=15, height=11, scale=1)
(gWucCnLbG4_plotly <- plot_ly(oemWUCDescpareto4, x=count, y=lbrHrs, group=WUC_Type, mode="markers", text=paste0(Work_Unit_Code_G4)) %>%
  layout(title="Work Unit Code Group Comparison - 4 Char - Overall Data", xaxis=list(title="Maintenance Actions"), yaxis=list(title="Labor Manhours")) )

# 2) Occurances
oemWUCDescparetoOrder <- as.character(oemWUCDescpareto[order(-oemWUCDescpareto$count),"Work_Unit_Code"]$Work_Unit_Code)
oemWUCDescparetoTop50 <- oemWUCDescpareto[oemWUCDescpareto$Work_Unit_Code %in% oemWUCDescparetoOrder[1:50],]
oemWUCDescparetoTop50 <- within(oemWUCDescparetoTop50, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(oemWUCDescparetoOrder,50))))
( gb <- ggplot(oemWUCDescparetoTop50, aes(x=Work_Unit_Code,y=count,fill=WUC_Type)) +
  geom_bar(stat="identity") + labs(x="Work Unit Code",y="Total Actions, OEM Data",title="Top 50 Work Unit Codes by Action Count") + coord_flip() +
  scale_fill_manual(values=c("lightblue","grey79","palegreen2")) +
  geom_text(cex=4,aes(hjust=0,x=Work_Unit_Code,y=0,label=paste(WUC_Narrative))) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) )
ggsave(filename="wuc_actions_pareto_oem_top50.svg", plot=gb, width=15, height=9, scale=1)
# 3) repair days
# distributions? total per week? average? total
# reorder & pareto
oemWUCDescparetoOrder <- as.character(oemWUCDescpareto[order(-as.numeric(oemWUCDescpareto$totalMaintDays)),"Work_Unit_Code"]$Work_Unit_Code)
oemWUCDescparetoTop50 <- oemWUCDescpareto[oemWUCDescpareto$Work_Unit_Code %in% oemWUCDescparetoOrder[1:50],]
oemWUCDescparetoTop50 <- within(oemWUCDescparetoTop50, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(oemWUCDescparetoOrder,50))))
(gb <- ggplot(oemWUCDescparetoTop50, aes(x=Work_Unit_Code,y=as.numeric(totalMaintDays),fill=WUC_Type)) +
  geom_bar(stat="identity") + labs(x="Work Unit Code",y="Total Maintenance Days, OEM Data",title="Top 50 Work Unit Codes by Maintenance Days, Overlapping JCNs") + 
  coord_flip() + scale_fill_manual(values=c("lightblue","grey79","palegreen2")) +
  geom_text(cex=4,aes(hjust=0,x=Work_Unit_Code,y=0,label=paste(WUC_Narrative))) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) ) # how to change the labels?
ggsave(filename="wuc_maintenance_Days_pareto_oem_top50.svg", plot=gb, width=15, height=9, scale=1)

### NOW WITH type-maintenance for scheduled and unscheduled - most jcn/wuc combos have only one type, BUT there's still some weird data, like 'radome, tail cone' comes up with a lot more days than before and 'actr, owf l&r' a lot fewer
### NOT UP TO DATE (11/13/2015)
oemWUCDescRepDaysparetoMType <- group_by(oem, Work_Unit_Code, WUC_Narrative, Job_Control_Number, Type_Maintenance_Code) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours), minDt = min(Transaction_Date), maxDt = max(Transaction_Date))
oemWUCDescRepDaysparetoMType$maintDays <- 1+difftime(oemWUCDescRepDaysparetoMType$maxDt,oemWUCDescRepDaysparetoMType$minDt,units="days")
oemWUCDescRepDaysparetoMType$maintType <- NA
oemWUCDescRepDaysparetoMType[oemWUCDescRepDaysparetoMType$Type_Maintenance_Code %in% c("A", "C", "D", "E", "H", "J", "P", "Q", "R"),]$maintType <- "Scheduled"
oemWUCDescRepDaysparetoMType[oemWUCDescRepDaysparetoMType$Type_Maintenance_Code %in% c("B", "S", "Y"),]$maintType <- "Unscheduled"
oemWUCDescRepDaysparetoMType$maintType<-factor(oemWUCDescRepDaysparetoMType$maintType)
oemWUCDescRepDaysparetoMType <- group_by(oemWUCDescRepDaysparetoMType,Work_Unit_Code,WUC_Narrative,maintType) %>% summarise(totalMaintDays = sum(maintDays)) # agg over JCN
# reorder & pareto
oemWUCDescRepDaysparetoMType$WUC_Narrative <- as.character(oemWUCDescRepDaysparetoMType$WUC_Narrative)
theOrder <- group_by(oemWUCDescRepDaysparetoMType, WUC_Narrative) %>% summarise(wucMntHr = sum(totalMaintDays))
theOrder <- theOrder[order(-as.numeric(theOrder$wucMntHr)),"WUC_Narrative"]
oemWUCDescRepDaysparetoMType <- within(oemWUCDescRepDaysparetoMType, WUC_Narrative <- factor(WUC_Narrative, levels = rev(theOrder$WUC_Narrative)))
gb <- ggplot(oemWUCDescRepDaysparetoMType, aes(x=WUC_Narrative, y=as.numeric(totalMaintDays))) + 
  geom_bar(stat="identity", aes(fill=maintType)) + labs(y="Total Maintenance Days by Type, OEM Data") + coord_flip()
gb  
ggsave(filename="wuc_maint_days_byType_pareto_oem_all.svg", plot=gb, width=15, height=40, scale=1)
# top 20
theOrder20 <- tail(levels(oemWUCDescRepDaysparetoMType$WUC_Narrative), 20)
oemWUCDescRepDaysparetoMType20 <- oemWUCDescRepDaysparetoMType[oemWUCDescRepDaysparetoMType$WUC_Narrative %in% theOrder20,]
gb <- ggplot(oemWUCDescRepDaysparetoMType20, aes(x=WUC_Narrative, y=as.numeric(totalMaintDays))) + 
  geom_bar(stat="identity", aes(fill=maintType)) + labs(y="Total Maintenance Days by Type, OEM Data") + coord_flip()
ggsave(filename="wuc_maint_days_byType_pareto_oem_top20.svg", plot=gb, width=10, height=8, scale=1)

############ Now mix with debrief data (by JCN and SN)
# mix oem with debrief for deviation code
# 2a) for all flights
# 2b) only for aborts
oemDebWUC_SNJCN <- inner_join(select(oem,Labor_Manhours,Job_Control_Number, Work_Unit_Code, WUC_Narrative,Serial_Number, Transaction_Date, How_Malfunction_Class_Ind), 
                                 select(debrief, Job_Control_Number, Deviation_Code, Subsystem_WUC_Description, Serial_Number), by=c("Job_Control_Number","Serial_Number"))
oemDebWUC_SNJCN <- group_by(oemDebWUC_SNJCN, Work_Unit_Code, WUC_Narrative, Deviation_Code, Subsystem_WUC_Description, Transaction_Date, How_Malfunction_Class_Ind) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours))
# find order & plot
oemDebWUC_SNJCN[,lapply(oemDebWUC_SNJCN,class)=="factor"] <- lapply(oemDebWUC_SNJCN[,lapply(oemDebWUC_SNJCN,class)=="factor"],factor)
oemDebWUC_SNJCNagg <- group_by(oemDebWUC_SNJCN, Work_Unit_Code) %>% summarise(count = sum(count), lbrHrs = sum(lbrHrs))
# add WUC Type
oemDebWUC_SNJCN$WUC_Type[oemDebWUC_SNJCN$Work_Unit_Code %in% gsWucs] <- "Support General"
oemDebWUC_SNJCN$WUC_Type[!(oemDebWUC_SNJCN$Work_Unit_Code %in% gsWucs)] <- "Bill of Material"
oemDebWUC_SNJCN$WUC_Type[is.na(oemDebWUC_SNJCN$Work_Unit_Code)] <- "None"
oemDebWUC_SNJCN$WUC_Type <- as.factor(oemDebWUC_SNJCN$WUC_Type)
# Add missing Work_Unit_Code and WUC_Narrative
oemDebWUC_SNJCN$Work_Unit_Code<-as.character(oemDebWUC_SNJCN$Work_Unit_Code);oemDebWUC_SNJCN$WUC_Narrative<-as.character(oemDebWUC_SNJCN$WUC_Narrative)
oemDebWUC_SNJCN$Work_Unit_Code[is.na(oemDebWUC_SNJCN$Work_Unit_Code)]<-"None";oemDebWUC_SNJCN$WUC_Narrative[is.na(oemDebWUC_SNJCN$WUC_Narrative)]<-"No Work Unit Code"
oemDebWUC_SNJCN$Work_Unit_Code<-factor(oemDebWUC_SNJCN$Work_Unit_Code);oemDebWUC_SNJCN$WUC_Narrative<-factor(oemDebWUC_SNJCN$WUC_Narrative)
# top 50
oemDebWUC_SNJCNOrder <- as.character(oemDebWUC_SNJCNagg[order(-oemDebWUC_SNJCNagg$count),"Work_Unit_Code"]$Work_Unit_Code)
oemDebWUC_SNJCNTop50 <- oemDebWUC_SNJCN[oemDebWUC_SNJCN$Work_Unit_Code %in% oemDebWUC_SNJCNOrder[1:50],]
oemDebWUC_SNJCNTop50 <- within(oemDebWUC_SNJCNTop50, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(oemDebWUC_SNJCNOrder,50))) )
( gb <- ggplot(oemDebWUC_SNJCNTop50, aes(x=Work_Unit_Code, y=count)) + 
  geom_bar(stat="identity", aes(fill=Deviation_Code)) + labs(y="Total Maintenance Actions by WUC & Deviation Code, Joined Data") + coord_flip() )
ggsave(filename="wuc_maint_actions_byType_DevCode_pareto_joined_all.svg", plot=gb, width=15, height=40, scale=1)
# by How Malfunction Class
oemDebWUC_SNJCNTop50<-oemDebWUC_SNJCNTop50[order(oemDebWUC_SNJCNTop50$How_Malfunction_Class_Ind),] # so that all HWC are together within each bar
( gb <- ggplot(oemDebWUC_SNJCNTop50, aes(x=Work_Unit_Code, y=count,fill=How_Malfunction_Class_Ind)) + ggtitle("Total Maintenance Actions by WUC & How-Malfunction Class, Joined Data (GenSup WUCS drop out)") +
    geom_bar(stat="identity") + labs(x="Work Unit Code",y="Total Maintenance Actions") +
  coord_flip() + theme_bw() + geom_text(font=1,cex=3,aes(hjust=0,x=Work_Unit_Code,y=0,label=paste(WUC_Narrative))) +
  scale_fill_manual(values=c("palevioletred","limegreen","cyan3"),labels=c("1 - Inherent","2 - Induced","6 - No Defect"),guide=guide_legend(title="How Malfunction Class")) + 
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()))
ggsave(filename="wuc_maint_actions_byHowMalClass_pareto_joined_top50.svg", plot=gb, width=12, height=16, scale=1)

#### only aborted ones
oemDebWUC_SNJCNabort <- filter(oemDebWUC_SNJCN, Deviation_Code %in% c('GA','AI','AA'))
oemDebWUC_SNJCNabort$abortType <- "Air" # oemWUCDescparetoDCabort
oemDebWUC_SNJCNabort[oemDebWUC_SNJCNabort$Deviation_Code %in% 'GA',]$abortType <- "Ground"
# sort levels
# filter out non-existant WUCs
oemDebWUC_SNJCNabort[,lapply(oemDebWUC_SNJCNabort,class)=="factor"] <- lapply(oemDebWUC_SNJCNabort[,lapply(oemDebWUC_SNJCNabort,class)=="factor"],factor)
oemDebWUC_SNJCNabortagg <- group_by(oemDebWUC_SNJCNabort, Work_Unit_Code) %>% summarise(count = sum(count), lbrHrs = sum(lbrHrs))
oemDebWUC_SNJCNabort <- within(oemDebWUC_SNJCNabort, WUC_Narrative <- factor(WUC_Narrative, levels = levels(oemDebWUC_SNJCNabort$WUC_Narrative)[order(as.numeric(oemDebWUC_SNJCNabortagg$count))]))
( gb <- ggplot(oemDebWUC_SNJCNabort, aes(x=WUC_Narrative, y=count)) + ggtitle("Total Maintenance Actions from Aborted Flights by WUC and Abort Type, Joined Data") + 
    geom_bar(stat="identity", aes(fill=abortType)) + labs(y="Maintenance Actions from Aborted Flights, Joined Data",fill="Abort Type") + coord_flip() )
ggsave(filename="wuc_maint_actions_AbortType_pareto_joined_all.svg", plot=gb, width=11, height=8, scale=1) 
## Connection between debrief subsystem and eventual WUC - way too many of these subystems
( gb <- ggplot(oemDebWUC_SNJCN, aes(x=WUC_Narrative, y=count)) + ggtitle("Total Maintenance Actions by WUC & Debrief Subsystem, Joined Data") +
    geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Maintenance Actions") + coord_flip() )
ggsave(filename="wuc_maint_actions_debSubSys_pareto_joined_all.svg", plot=gb, width=20, height=40, scale=1)
# connection of just aborted ones
( gb <- ggplot(oemDebWUC_SNJCNabort, aes(x=WUC_Narrative, y=count)) + ggtitle("Maintenance Actions from Aborted Sorties by WUC & Debrief Subsystem, Joined Data") +
    geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Maintenance Actions") + coord_flip() )
ggsave(filename="wuc_maint_actions_aborted_DebSubsys_pareto_joined_all.svg", plot=gb, width=11, height=8, scale=1) 
# facet
( gb <- ggplot(oemDebWUC_SNJCNabort, aes(x=WUC_Narrative, y=count)) + ggtitle("Maintenance Actions from Aborted Sorties by WUC, Abort Type, Debrief Subsystem, Joined Data") +
    geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Maintenance Actions") + coord_flip() + facet_grid( . ~ abortType) )
ggsave(filename="wuc_maint_actions_AbortType_DebSubsys_pareto_joined_all.svg", plot=gb, width=18, height=8, scale=1) 
# top 50 - first by labor hours
oemDebWUC_SNJCNabortagg <- group_by(oemDebWUC_SNJCNabort, Work_Unit_Code) %>% summarise(count = sum(count), lbrHrs = sum(lbrHrs))
oemDebWUC_SNJCNabortOrder <- as.character(oemDebWUC_SNJCNabortagg[order(-oemDebWUC_SNJCNabortagg$lbrHrs),"Work_Unit_Code"]$Work_Unit_Code)
oemDebWUC_SNJCNabort50 <- oemDebWUC_SNJCNabort[oemDebWUC_SNJCNabort$Work_Unit_Code %in% oemDebWUC_SNJCNabortOrder[1:50],]
oemDebWUC_SNJCNabort50 <- within(oemDebWUC_SNJCNabort50, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(oemDebWUC_SNJCNabortOrder,50))))
( gb <- ggplot(oemDebWUC_SNJCNabort50, aes(x=Work_Unit_Code, y=lbrHrs)) + ggtitle("Maintenance Actions from Aborted Sorties by WUC & Debrief Subsystem, Joined Data") +
  geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Labor Hours") + coord_flip() + theme_bw() )
( gb <- ggplot(oemDebWUC_SNJCNabort50, aes(x=Work_Unit_Code, y=lbrHrs)) + ggtitle("Maintenance Labor Hours from Aborted Sorties by Work Unit Code & Abort Type, Joined Data") +
  geom_bar(stat="identity", aes(fill=abortType)) + labs(x="Work Unit Code",y="Labor Hours") + coord_flip() + theme_bw() +
  geom_text(cex=4,aes(hjust=0,x=Work_Unit_Code,y=0,label=paste(WUC_Narrative))) + 
  scale_fill_manual(name="Abort Type",values=c("lightblue","palegreen2")) + 
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) )
ggsave("wuc_lbrHrs_AbortType_pareto_joined_top50.svg",gb,width=16, height=10, scale=1)
# actions
oemDebWUC_SNJCNabortOrder <- as.character(oemDebWUC_SNJCNabortagg[order(-oemDebWUC_SNJCNabortagg$count),"Work_Unit_Code"]$Work_Unit_Code)
oemDebWUC_SNJCNabort50 <- oemDebWUC_SNJCNabort[oemDebWUC_SNJCNabort$Work_Unit_Code %in% oemDebWUC_SNJCNabortOrder[1:50],]
oemDebWUC_SNJCNabort50 <- within(oemDebWUC_SNJCNabort50, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(oemDebWUC_SNJCNabortOrder,50))))
( gb <- ggplot(oemDebWUC_SNJCNabort50, aes(x=Work_Unit_Code, y=count)) + ggtitle("Maintenance Actions from Aborted Sorties by Work Unit Code & Abort Type, Joined Data") +
  geom_bar(stat="identity", aes(fill=abortType)) + labs(x="Work Unit Code",y="Actions") + coord_flip() + theme_bw() +
  geom_text(cex=4,aes(hjust=0,x=Work_Unit_Code,y=0,label=paste(WUC_Narrative))) + 
  scale_fill_manual(name="Abort Type",values=c("lightblue","palegreen2")) + 
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) )
ggsave("wuc_actions_AbortType_pareto_joined_top50.svg",gb,width=16, height=10, scale=1)
# facet
( gb <- ggplot(oemDebWUC_SNJCNabort50, aes(x=Work_Unit_Code, y=lbrHrs)) + ggtitle("Maintenance Actions from Aborted Sorties by WUC & Debrief Subsystem, Joined Data") +
  geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Labor Hours") + coord_flip() + theme_bw() + facet_grid( . ~ abortType) )
# by wuc type - they're all Bill of Material
( gb <- ggplot(oemDebWUC_SNJCNabort50, aes(x=Work_Unit_Code, y=lbrHrs)) + ggtitle("Maintenance Actions from Aborted Sorties by WUC & Debrief Subsystem, Joined Data") +
  geom_bar(stat="identity", aes(fill=WUC_Type)) + labs(y="Labor Hours") + coord_flip() + theme_bw() )
#### Now connect tables via Subsystem WUCs - these are three digits
oemDebWUC_SNJCN <- inner_join(select(oem,Labor_Manhours,Job_Control_Number, Work_Unit_Code, WUC_Narrative,Serial_Number, Transaction_Date, How_Malfunction_Class_Ind), 
                              select(debrief, Job_Control_Number, Deviation_Code, Subsystem_WUC_Description,Serial_Number, Work_Unit_Code, WUC_Description), by=c("Job_Control_Number","Serial_Number"))
oemDebWUC_SNJCN <- group_by(oemDebWUC_SNJCN, Work_Unit_Code, WUC_Narrative, Deviation_Code, Subsystem_WUC_Description, Transaction_Date, How_Malfunction_Class_Ind) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours))


# PN pareto
## by part number
# top five WUCs, faceted - this doesn't look great, but it is not that useful : we don't need to compare PNs across WUCs
oemWUCDescparetoDC5 <- oemWUCDescparetoDC[oemWUCDescparetoDC$WUC_Narrative %in% tail(levels(oemWUCDescparetoDC$WUC_Narrative),5),]
# relevel all factors in the DF
oemWUCDescparetoDC5[,lapply(oemWUCDescparetoDC5,class)=="factor"] <- lapply(oemWUCDescparetoDC5[,lapply(oemWUCDescparetoDC5,class)=="factor"],factor)
( gb <- ggplot(oemWUCDescparetoDC5, aes(x=On_Component_Part_Number, y=count)) + ggtitle("Total Maintenance Actions by WUC & How-Malfunction Class, Joined Data") +
    geom_bar(stat="identity", aes(fill=How_Malfunction_Class_Ind)) + labs(y="Total Maintenance Actions") + scale_fill_discrete(labels=c("1 - Inherent","2 - Induced","6 - No Defect")) + facet_grid(.~WUC_Narrative, scales="free") ) # coord_flip doesn't play nicely with this
# on WUC
( gb <- ggplot(oemWUCDescparetoDC5, aes(x=On_Component_Part_Number, y=count)) + ggtitle("Total Maintenance Actions for WUC SDMA & How-Malfunction Class, Joined Data") +
    geom_bar(stat="identity", aes(fill=How_Malfunction_Class_Ind)) + labs(y="Total Maintenance Actions") + scale_fill_discrete(labels=c("1 - Inherent","2 - Induced","6 - No Defect")) + coord_flip() ) 

######### AWP
# add subtract of removals, add dates of installs
# or have 1/-1 coding for T and U and do sumproduct
awpDates <- group_by(oem, Serial_Number, Job_Control_Number, Action_Taken_Code,Geographic_Location) %>% filter(Action_Taken_Code %in% c('T','U')) %>% select(Serial_Number, Job_Control_Number, Action_Taken_Code, Transaction_Date, Work_Unit_Code, WUC_Narrative, Sequence_Number,Geographic_Location)
awpDates <- awpDates[order(awpDates$Serial_Number,awpDates$Job_Control_Number,awpDates$Work_Unit_Code,awpDates$Transaction_Date),]
# condition data for June/July 2014
awpDates$ATCcode <- -1;awpDates[awpDates$Action_Taken_Code %in% "U",]$ATCcode <- 1
# 1) group successive installs and removals into one, and take the latest date: use Sequence Number
awpDates <- mutate(awpDates, maxActSeqNum = max(Sequence_Number)) # remove records that aren't the last one in the action/wuc/sn group
awpDates <- awpDates[awpDates$Sequence_Number == awpDates$maxActSeqNum,] #  - 29 rows, or 5%
awpDates$maxActSeqNum <- NULL
###### ASSUMES JUNE / JULY DATA ONLY

# 2) fix removals without an install / installs without a removal - add an install on August 1st / removal on May 30th
awpDates <- mutate(awpDates, actCtPerTypeJCN = n())
awpDates[awpDates$actCtPerTypeJCN > 1 ,] # - none, that's good - each SN/JCN/WUC has 1 or fewer removals 1 or fewer installs
awpDates <- ungroup(awpDates) %>% group_by(Serial_Number, Job_Control_Number) %>% mutate(actCtPerJCN = n())
# fix removals first - add an install
# I really should fix THIS   data before I do this
awpDates[awpDates$actCtPerJCN == 1,names(awpDates)[-6]]
# for now I'm going to ignore it, except..
# nearly all of this is in WWYK, which doesn't keep good records
# I'll fix the one record that isn't - 
tempAWP <- awpDates[awpDates$actCtPerJCN == 1 & !(awpDates$Geographic_Location %in% "WWYK"),]
tempAWP$Action_Taken_Code <- ifelse(tempAWP$Action_Taken_Code %in% "T","U","T") # switch action taken
tempAWP$ATCcode <- ifelse(tempAWP$Action_Taken_Code %in% "T","-1","1") # switch action taken code
tempAWP$actCtPerJCN <- 2
if(tempAWP$Action_Taken_Code %in% "T"){tempAWP$Transaction_Date <- as.POSIXct('2014-05-30',tz="UTC")} # ifelse doesn't keep the class
if(tempAWP$Action_Taken_Code %in% "U"){tempAWP$Transaction_Date <- as.POSIXct('2014-08-01',tz="UTC")} # safe.ifelse doesn't keep the timezone
awpDates <- rbind(awpDates,tempAWP) # attach to main table
awpDates[awpDates$actCtPerJCN == 1 & !(awpDates$Geographic_Location %in% "WWYK"),"actCtPerJCN"] <- 2 # fix the matched records
# 3) removal before June 1st, Install after June 1st - add removal at June 1st and
awpDates[awpDates$actCtPerJCN == 2 & awpDates$Transaction_Date < '2014-05-30' & awpDates$Action_Taken_Code %in% "T","Transaction_Date"] <- ymd('2014-05-30',tz="UTC")
awpDates[awpDates$actCtPerJCN == 2 & awpDates$Transaction_Date > '2014-08-01' & awpDates$Action_Taken_Code %in% "U","Transaction_Date"] <- ymd('2014-08-01',tz="UTC")
plot(x=awpDates$Transaction_Date,y=awpDates$Serial_Number)
# 4) sum product ! - TOTAL AWP, meaning multiple-counting - a single SN could be AWP during the same day for two WUCs & double-counting
# converting date to numeric gives seconds since 1/1/1970
awpTotSec <- sum(as.numeric(awpDates[awpDates$actCtPerJCN == 2,]$Transaction_Date) * as.numeric(awpDates[awpDates$actCtPerJCN == 2,]$ATCcode))
awpTotSec/60/60/24/365.25 # years of downtime
awpDates$Serial_Number <- factor(awpDates$Serial_Number)
awpTotSec/60/60/24/365.25/62 # years per TN
awpTotSec/60/60/24/(30+31)/62 # fraction of two months that TN is down for awp (overlapping)
# 5) How many AWP days? group by UWC
convert to intervals with an install and removal date
# get rid of WWYK ones for now
awpIntervals <- awpDates[awpDates$actCtPerJCN == 2,]
awpIntervals1 <- awpIntervals[awpIntervals$Action_Taken_Code %in% "U",c(1,2,4,5,8)]
names(awpIntervals1) <- c("Serial_Number","Job_Control_Number","Replace_Date","Replaced_WUC","Rep_Geo_Loc")
awpIntervals2 <- awpIntervals[awpIntervals$Action_Taken_Code %in% "T",c(1,2,4,5,8)]
names(awpIntervals2) <- c("Serial_Number","Job_Control_Number","Removal_Date","Removed_WUC","Rem_Geo_Loc")
awpIntervals <- inner_join(awpIntervals2,awpIntervals1);rm(awpIntervals1);rm(awpIntervals2)
awpIntervals$AWPtime <- difftime(awpIntervals$Replace_Date,awpIntervals$Removal_Date,units="days")
plot(x=awpIntervals$Replace_Date,y=awpIntervals$Serial_Number,col="green",pch=16)
points(x=awpIntervals$Removal_Date,y=awpIntervals$Serial_Number,col="black",pch=1)
# group this by replacement WUC - this includes overlapping WUCs within a single SN, and maybe multiple days for the same WUC and SN (e.g. 75J99)
awpIntervalsByWUC <- group_by(awpIntervals,Replaced_WUC) %>% summarise(totalAWPDays = sum(AWPtime))
awpIntervalsByWUC <- left_join(awpIntervalsByWUC,select(oem,Work_Unit_Code, WUC_Narrative)%>% distinct(),by=c("Replaced_WUC"="Work_Unit_Code") )
awpIntervalsByWUC <- within(awpIntervalsByWUC, Replaced_WUC <- factor(awpIntervalsByWUC$Replaced_WUC,
                                         levels=awpIntervalsByWUC[order(awpIntervalsByWUC$totalAWPDays,decreasing=FALSE),]$Replaced_WUC))
awpIntervalsByWUC50 <- awpIntervalsByWUC[awpIntervalsByWUC$Replaced_WUC %in% tail(levels(awpIntervalsByWUC$Replaced_WUC),50),]
(gpWawp <- ggplot(awpIntervalsByWUC50, aes(x=Replaced_WUC,y=as.numeric(totalAWPDays))) +
  geom_bar(stat="identity",fill="lightblue") + labs(x="Replacement Work Unit Code",y="Total B1 Days Awaiting Work Unit Code, OEM Data",title="Work Unit Code Drivers of Awaiting Parts Days, June/July 2014") + 
  coord_flip() + geom_text(cex=4,aes(hjust=0,x=Replaced_WUC,y=0,label=paste(WUC_Narrative))) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) )

# 6) How many days was each SN AWP?
# bring in that big SN/Day table to assign SNs to either AWP or not
head(lifetimes)
SNDate<-lifetimes[,c(1,2)]
SNDate$awp<-FALSE

######### Parts requirement (Action Taken R - remove and replace with like item)

######### attribute maintenance hours to particular sorties
sortieHrs[1,] # has flown hours of each sortie (maybe zero)
sortieMaint <- group_by(oem, Job_Control_Number, Serial_Number) %>% summarise(jobLaborHrs = sum(Labor_Manhours))
sortieMaint <- select(debrief, Job_Control_Number, Sortie_Number, Sortie_Date, Serial_Number, Deviation_Code, Mission_Code, Mission_Class, Geographic_Location) %>% inner_join(sortieMaint, by=c("Serial_Number","Job_Control_Number")) %>% group_by(Sortie_Number, Sortie_Date, Serial_Number, Deviation_Code, Mission_Code, Mission_Class, Geographic_Location) %>% summarise(sortieLaborHrs = sum(jobLaborHrs))
sortieHrsAndMaint <- full_join(sortieHrs, sortieMaint, by=c("Sortie_Number", "Serial_Number", "Sortie_Date","Mission_Code","Mission_Class"))
# set NAs to zero
sortieHrsAndMaint[is.na(sortieHrsAndMaint$sortieLaborHrs),]$sortieLaborHrs <- 0
(gd <- ggplot(sortieHrsAndMaint, aes(x=maxFlHr, y=sortieLaborHrs)) + geom_point(size=2.5) + labs(x="Sortie Flight Hours",y="Labor Hours From Sortie",title="Flight and Labor Hours Per Attempted Sortie"))
ggsave(filename="sortie_flightHrs_laborHrs.svg", plot=gd, width=15,height=12)
(gd <- ggplot(sortieHrsAndMaint, aes(x=maxFlHr, y=sortieLaborHrs, colour=Mission_Class)) + geom_point(size=2.5) + labs(x="Sortie Flight Hours",y="Labor Hours From Sortie",title="Flight and Labor Hours Per Attempted Sortie") +
  theme_bw() + theme(panel.border = element_blank(),axis.line = element_line(color = 'black')) )
ggsave(filename="sortie_flightHrs_laborHrs_byMisClass.svg", plot=gd, width=15,height=12)
(gd <- ggplot(sortieHrsAndMaint, aes(x=maxFlHr, y=sortieLaborHrs, colour=Deviation_Code)) + geom_point(size=2.5) + labs(x="Sortie Flight Hours",y="Labor Hours From Sortie",title="Flight and Labor Hours Per Attempted Sortie"))
ggsave(filename="sortie_flightHrs_laborHrs_byDevCd.svg", plot=gd, width=15,height=12)
sortieHrsAndMaintPlotly <- sortieHrsAndMaint # make a new DF b/c I need an NA
sortieHrsAndMaintPlotly$Deviation_Code <- addNA(sortieHrsAndMaintPlotly$Deviation_Code)
sortieHrsAndMaintPlotly$Geographic_Location <- addNA(sortieHrsAndMaintPlotly$Geographic_Location)
sortieHrsAndMaintPlotly[is.na(sortieHrsAndMaintPlotly$sortieLaborHrs),]$sortieLaborHrs <- 0
(gdp <- plot_ly(sortieHrsAndMaintPlotly, x=maxFlHr, y=sortieLaborHrs, color=Deviation_Code, mode="markers", text=paste("SN:",Serial_Number,"on",Sortie_Date,"Mission Code:",Mission_Code,"Loc:",Geographic_Location)) %>% 
  layout(title="Sortie Flight and Labor Hours by Deviation Code, All Dates", xaxis=list(title="Sortie Flight Hours"), yaxis=list(title="Sortie Labor Manhours")))
(gdp <- plot_ly(sortieHrsAndMaintPlotly, x=maxFlHr, y=sortieLaborHrs, color=Geographic_Location, mode="markers", text=paste("SN:",Serial_Number,"on",Sortie_Date,"Mission Code:",Mission_Code,"Loc:",Geographic_Location)) %>% 
  layout(title="Sortie Flight and Labor Hours by Location, All Dates", xaxis=list(title="Sortie Flight Hours"), yaxis=list(title="Sortie Labor Manhours")))

######### attribute maintenance hours to particular serial numbers (even those not matching debrief data)
# reduce previous sortieHours and Maintenance to by Tail Number and Day
tnMaintByDay <- group_by(oem, Serial_Number, Transaction_Date) %>% summarise(tailLaborHours = sum(Labor_Manhours))
tnHrsByDay <- ungroup(sortieHrs) %>% group_by(Serial_Number,Sortie_Date) %>% summarise(tailFlightHours = sum(maxFlHr))
# now maintenance times
names(tnMaintByDay) <- c(names(tnMaintByDay)[1],"Event_Date",names(tnMaintByDay)[3])
names(tnHrsByDay) <- c(names(tnHrsByDay)[1],"Event_Date",names(tnHrsByDay)[3])
tnHrsAndMaintbyDay <- full_join(tnMaintByDay,tnHrsByDay,by=c("Serial_Number","Event_Date"))
tnHrsAndMaintbyDay$tailLaborHours[is.na(tnHrsAndMaintbyDay$tailLaborHours)]<-0
tnHrsAndMaintbyDay$tailFlightHours[is.na(tnHrsAndMaintbyDay$tailFlightHours)]<-0
tnHrsAndMaintAll <- group_by(tnHrsAndMaintbyDay, Serial_Number) %>% summarise(tailLaborHours = sum(tailLaborHours), tailFlightHours = sum(tailFlightHours))
# add geographic location - try Debrief data, then OEM data for missing ones (without sorties) b/c sometimes these have two geo locs
tnGeoLoc <- select(debrief,Serial_Number, Geographic_Location) %>% distinct()
tnGeoLoc <- tnGeoLoc[!(tnGeoLoc$Geographic_Location %in% "LUXC"),] # remove the LUXC
tnGeoLoc$Geographic_Location <- factor(tnGeoLoc$Geographic_Location)
tnHrsAndMaintAll <- left_join(tnHrsAndMaintAll, tnGeoLoc, by="Serial_Number")
tnGeoLocMaint <- group_by(oem,Serial_Number, Geographic_Location) %>% filter(!(Geographic_Location %in% "WWYK")) %>% summarise(count = n()) %>% mutate(groupMaxCount = max(count))
tnGeoLocMaint <- tnGeoLocMaint[tnGeoLocMaint$count == tnGeoLocMaint$groupMaxCount,] # reduces by three rows
# join to table, udpate, then remove
tnHrsAndMaintAll <- left_join(tnHrsAndMaintAll,select(tnGeoLocMaint,Serial_Number,Geographic_Location),by="Serial_Number")
tnHrsAndMaintAll[is.na(tnHrsAndMaintAll$Geographic_Location.x),]$Geographic_Location.x <- tnHrsAndMaintAll[is.na(tnHrsAndMaintAll$Geographic_Location.x),]$Geographic_Location.y
names(tnHrsAndMaintAll)[4]<-"Geographic_Location"; # rename
tnHrsAndMaintAll$Geographic_Location.y <- NULL# drop
# one plot for all data, one point for each TN
(gd <- ggplot(tnHrsAndMaintAll, aes(x=tailFlightHours, y=tailLaborHours, colour=Geographic_Location)) + 
  geom_point(size=2.5) + labs(x="June/July Accrued Flight Hours",y="Labor Hours In all Data",title="Flight and Labor Hours Per Tail Number") +
  scale_colour_discrete(labels=c("FNWZ - Dyess AFB","FSPM - Edwards AFB","FXBM - Ellsworth AFB")) + theme_bw() +
  theme(panel.border = element_blank(),axis.line = element_line(color = 'black')) )
ggsave(filename="tail_number_laborHrs_flightHrs_allData.svg", plot=gd, width=12, height=8)
(gdp <- plot_ly(tnHrsAndMaintAll, x=tailFlightHours, y=tailLaborHours, color=Geographic_Location, mode="markers", text=paste("SN:",Serial_Number)) %>%
  layout(title="Tail Number Flight and Labor Hours, All Dates", xaxis=list(title="Accrued Flight Hours"), yaxis=list(title="Total Labor Manhours")))
# I want to change legend text - I could either change data or use traces AFAIK
levels(tnHrsAndMaintAll$Geographic_Location) <- c("FNWZ:Dyess","FSPM:Edwards","FXBM:Ellsworth")
(gdp <- plot_ly(tnHrsAndMaintAll, x=tailFlightHours, y=tailLaborHours, color=Geographic_Location, mode="markers", text=paste("SN:",Serial_Number)) %>%
  layout(title="Tail Number Flight and Labor Hours, All Dates", xaxis=list(title="Accrued Flight Hours"), yaxis=list(title="Total Labor Manhours")))
# only June and July maintenance
# first reduce data to only June and July - will exclude maintenance on sorties at the end
tnHrsAndMaintbyDayJJ <- tnHrsAndMaintbyDay[tnHrsAndMaintbyDay$Event_Date < '2014-08-01' & tnHrsAndMaintbyDay$Event_Date > '2014-05-31',]
# facet by month - June and July

# facet by week - June and July


## How Malfunction Code - frequency and labor hours
howMalDictionary <- sqlQuery(udri, "SELECT * FROM how_malfunction_code")
colnames(howMalDictionary) <- c("How_Malfunction_Code","How_Malfunction_Class_Ind","description")
howMalDictionary$How_Malfunction_Code <- as.factor(howMalDictionary$How_Malfunction_Code)
howMalDictionary$How_Malfunction_Class_Ind <- as.factor(howMalDictionary$How_Malfunction_Class_Ind)
howMalClassDictionary <- data.frame("How_Malfunction_Class_Ind"=as.factor(c(1,2,6)),"Class_Description"=c("Inherent","Induced","No Defect"))
howMalDictionary <- left_join(howMalDictionary,howMalClassDictionary)
howMal <- group_by(oem, How_Malfunction_Code,How_Malfunction_Class_Ind) %>% 
  summarise(count = n(), lbrHrs = sum(Labor_Manhours)) %>% left_join(howMalDictionary)
# many how malfunction codes are missing and show '0' (20%)
(ghm <- ggplot(howMal, aes(x=count,y=lbrHrs)) + labs(x="Maintenance Actions",y="Labor Manhours",title="How Malfunction Code") + # text is for plotly
    geom_point(aes(text=paste("Code:",How_Malfunction_Code,"Class:",How_Malfunction_Class_Ind,":",description))))

## Fix Time
# time between landing and end of last maintenance action
parseJCNtimestamp <- function(datepart,hourminutepart){
  if(sum(is.na(c(datepart,hourminutepart))) > 0) {return(NA)}
  if(!between(nchar(hourminutepart),2,4)) {return(NA)}
  if(nchar(hourminutepart)==4) {
    timeElement = paste0(substr(hourminutepart,1,2),":",substr(hourminutepart,3,4))
  }
  if(nchar(hourminutepart)==3) {
    timeElement = paste0("0",substr(hourminutepart,1,1),":",substr(hourminutepart,2,3))
  }
  if(nchar(hourminutepart)==2) {
    timeElement = paste0("00:",substr(hourminutepart,1,2))
  }
  dateElement = substr(datepart,1,10)
  return(ymd_hm(paste(dateElement,timeElement)))
}
# what I had in SQL
# these are only the sorties resulting in breaks (landing status = 3) that flew
aFromSQL <- sqlQuery(udri, "SELECT sortie_number, sortie_date, MAX(hoursBetweenBreakAndFix) AS jcnMaxHoursBwBreakAndFix, COUNT(*) AS qtyJCNs FROM (SELECT lastJCNFixTimestampTable.sortie_number, lastJCNFixTimestampTable.sortie_date, lastJCNFixTimestampTable.lastJCNFixTimestamp, lastJCNLandTimestampTable.lastJCNLandTimestamp, lastJCNLandTimestampTable.job_control_number, TIMESTAMPDIFF(HOUR, lastJCNLandTimestampTable.lastJCNLandTimestamp, lastJCNFixTimestampTable.lastJCNFixTimestamp) AS hoursBetweenBreakAndFix FROM (SELECT sortie_number, sortie_date, job_control_number, MAX(TIMESTAMP(Landing_Date, CONCAT(landing_time, '00'))) AS lastJCNLandTimestamp FROM debrief GROUP BY job_control_number HAVING job_control_number IS NOT NULL) lastJCNLandTimestampTable JOIN (SELECT JCNs.sortie_number, JCNs.sortie_date, JCNs.job_control_number, MAX(TIMESTAMP(oem.transaction_date, CONCAT(oem.stop_time, '00'))) AS lastJCNFixTimestamp FROM (SELECT job_control_number, transaction_date, stop_time FROM on_equipment_maintenance) AS oem JOIN (SELECT DISTINCT d1.Sortie_Number, d1.Sortie_Date, d1.Job_Control_Number FROM debrief d1 JOIN (SELECT Sortie_Number, Sortie_Date, Landing_Status, SUM(flight_duration) fd FROM debrief WHERE Landing_Status = 3 GROUP BY Sortie_Number , Sortie_Date HAVING SUM(flight_duration) > 0) AS sortiesFlown ON sortiesFlown.sortie_number = d1.sortie_number AND sortiesFlown.sortie_date = d1.sortie_date WHERE Job_Control_Number IS NOT NULL) AS JCNs ON JCNs.job_control_number = oem.job_control_number GROUP BY JCNs.sortie_number , JCNs.sortie_date , JCNs.job_control_number) lastJCNFixTimestampTable ON lastJCNFixTimestampTable.job_control_number = lastJCNLandTimestampTable.job_control_number GROUP BY lastJCNLandTimestampTable.Job_Control_Number) AS jcnFixTimeTable GROUP BY Sortie_Number , Sortie_Date")
( ggplot(aFromSQL, aes(x=jcnMaxHoursBwBreakAndFix)) + geom_histogram(stat="bin") + labs(x="Time to Fix, Hours",y="Sorties",title="Sortie Breaks : Fix Time"))
( ggplot(aFromSQL, aes(x=qtyJCNs)) + geom_histogram(stat="bin") + labs(x="JCNs Generated from Sortie",y="Sorties",title="Sortie Breaks : JCNs"))
( gd <- ggplot(aFromSQL, aes(x=qtyJCNs,jcnMaxHoursBwBreakAndFix)) + geom_point() + 
  labs(x="JCNs Generated from Sortie",y="Time to Fix All JCNs within Sortie, Hours",title="Sortie Breaks : JCNs and Fix Time"))
ggsave(filename="sortie_JCNs_fixTime_scatter.svg", plot=gd, width=12, height=8)

# A) by "flown sortie"
attemptedSortieLandingTimes <- group_by(debrief, Serial_Number,Sortie_Number, Sortie_Date, Landing_Time) %>% distinct() %>% select(Sortie_Number, Sortie_Date, Landing_Time)


# B) by JCN & SN
lastDebriefJCN <- group_by(debrief, Job_Control_Number, Serial_Number, Landing_Date, Landing_Time) %>% select(Job_Control_Number, Serial_Number, Landing_Date, Landing_Time) %>% distinct()
lastDebriefJCN$Landing_DateTime <- 

JCNfixTime <- group_by(debrief, Job_Control_Number, Serial_Number) %>% 
  
  left_join(select(oem, Job_Control_Number, Serial_Number, Trans))

### Fleet Metrics: Operations and Maintenance
# Operations
#weeklyEvents <- 

#### maintenance dashboard
# actions, labor hours, labor hours per action - by location ;  need # maintainers to do labor hours per maintainer
maintActions <- group_by(oem, Transaction_Date, Geographic_Location) %>% summarise(count = n(), laborHours = sum(Labor_Manhours), lbrHrPerAct = sum(Labor_Manhours)/n())
maintActions <- maintActions[maintActions$Transaction_Date > '2014-05-31' & maintActions$Transaction_Date < '2014-08-01',]
( gmt <- ggplot(maintActions, aes(x=Transaction_Date, y=count, colour=Geographic_Location)) + geom_line() )
( gmt <- ggplot(maintActions, aes(x=Transaction_Date, y=laborHours, colour=Geographic_Location)) + geom_line() )
( gmt <- ggplot(maintActions, aes(x=Transaction_Date, y=lbrHrPerAct, colour=Geographic_Location)) + geom_line() + labs(y="Labor Hours Per Action"))
ggsave(filename="maintenance_labor_hours_per_action_by_geoLoc_Date_JunJul.svg", plot=gmt, width=12, height=8)
( gmtp <- plot_ly(maintActions, x=Transaction_Date, y=lbrHrPerAct, color=Geographic_Location) )
( gmtp <- plot_ly(maintActions, x=Transaction_Date, y=laborHours, color=Geographic_Location) )
maintActions <- maintActions %>% gather(metric, value, 3:5) # make data frame tidy
( gmt <- ggplot(maintActions, aes(x=Transaction_Date, y=value, colour=Geographic_Location)) + geom_line() + facet_grid(metric~.,scale="free_y") )
ggsave(filename="maintenance_labor_hours_metrics_by_geoLoc_Date_JunJul.svg", plot=gmt, width=12, height=8)
tnHrsAndMaintbyDayJJ <- tnHrsAndMaintbyDay[tnHrsAndMaintbyDay$Event_Date < '2014-08-01' & tnHrsAndMaintbyDay$Event_Date > '2014-05-31',]

#########
## Work order distribution
workOrders <- select(oem, Work_Order_Number, Geographic_Location, Transaction_Date, Labor_Manhours) %>% group_by(Work_Order_Number)
workOrders <-  summarise(workOrders, totalHrs = sum(Labor_Manhours), maxDate = max(Transaction_Date), minDate = min(Transaction_Date), maxDate = max(Transaction_Date), minDate = min(Transaction_Date), minDt = min(Transaction_Date), maxDt = max(Transaction_Date))
workOrders$daysSpan <- workOrders$maxDate - workOrders$minDate
# I can't use qplot for work order days because its discrete and it looks awful
#barplot(table(as.numeric(workOrders$daysSpan)),xlim=c(0,5), xlab="Work Order Total Days To Complete") - x axis is awful
plot(table(as.numeric(workOrders$daysSpan)),xlim=c(0,14),ylab="count", xlab="Work Order Total Days To Complete", main="Calendar Time", frame.plot=FALSE)
hist(workOrders$totalHrs,xlim=c(0,40),breaks=50000/30, xlab="Work Order Total Labor Hours To Complete", ylab="count", main="Labor Hours")
#qplot(workOrders$totalHrs,xlim=c(0,50), xlab="Work Order Total Labor Hours To Complete")
#(ggplot(workOrders, aes(y=totalHrs)) + geom_boxplot())
#plot(density(workOrders$totalHrs))
# exported at 597 x 378

##################### APPENDIX L
## 1) ABORT AIR
## if DEBRIEF DEVIATION CODE = "AA" OR "AI", then add "1" to AIR ABORT.
(aaTotal<-sqlQuery(udri, "SELECT count(*) as airAborts FROM debrief WHERE Deviation_Code in ('AA','AI')")) #sum(debrief$Deviation_Code %in% c("AA","AI"))

## 2) ABORT AIR RELATED MAINTENANCE ACTIONS
## if WHEN DISCOVERED CODE = "C" then add units to AIR ABORT RELATED MAINTENANCE ACTIONS.

## 3) ABORT AIR RATE
## ( ABORT AIR / SORTIES FLOWN ) x 100
sortiesTotal <- 
aaRate <- 
aaRatioToSortie<-aaTotal/length(unique(debrief))

##################### analytics from sql file
# debrief WUCs pareto - Air abort
debWUCpareto<-sqlQuery(udri, "SELECT 
    Subsystem_Work_Unit_Code,
    Subsystem_WUC_Description,
                       COUNT(*) AS airAborts
                       FROM
                       (SELECT 
                       d.Sortie_Number,
                       d.Sortie_Date,
                       d.Deviation_Code,
                       d.Subsystem_Work_Unit_Code,
                       d.Subsystem_WUC_Description
                       FROM
                       debrief d
                       WHERE
                       d.Deviation_Code IN ('AA' , 'AI')
                       GROUP BY d.Sortie_Number , d.Sortie_Date , deviation_code , Subsystem_Work_Unit_Code , Subsystem_WUC_Description) AS allAirAbortSubWucs
                       GROUP BY Subsystem_Work_Unit_Code , Subsystem_WUC_Description
                       ORDER BY COUNT(*) DESC")
debWUCpareto <- within(debWUCpareto, Subsystem_WUC_Description <- factor(
  Subsystem_WUC_Description, levels = rev(as.character(debWUCpareto$Subsystem_WUC_Description))))
gb <- ggplot(debWUCpareto, aes(x=Subsystem_WUC_Description,y=airAborts)) +
  geom_bar(stat="identity") + coord_flip()
gb
ggsave("subsystem_wuc_air_aborted_sorties_pareto_debrief.svg",scale=2)
## ground aborts
debWUCpareto<-sqlQuery(udri, "SELECT 
    Subsystem_Work_Unit_Code,
    Subsystem_WUC_Description,
                       COUNT(*) AS groundAborts
                       FROM
                       (SELECT 
                       d.Sortie_Number,
                       d.Sortie_Date,
                       d.Deviation_Code,
                       d.Subsystem_Work_Unit_Code,
                       d.Subsystem_WUC_Description
                       FROM
                       debrief d
                       WHERE
                       d.Deviation_Code LIKE 'GA'
                       GROUP BY d.Sortie_Number , d.Sortie_Date , deviation_code , Subsystem_Work_Unit_Code , Subsystem_WUC_Description) AS allAirAbortSubWucs
                       GROUP BY Subsystem_Work_Unit_Code , Subsystem_WUC_Description
                       ORDER BY COUNT(*) DESC")
debWUCpareto <- within(debWUCpareto, Subsystem_WUC_Description <- factor(
  Subsystem_WUC_Description, levels = rev(as.character(debWUCpareto$Subsystem_WUC_Description))))
gb <- ggplot(debWUCpareto, aes(x=Subsystem_WUC_Description,y=groundAborts)) +
  geom_bar(stat="identity") + coord_flip()
gb
ggsave("subsystem_wuc_ground_aborted_sorties_pareto_debrief.svg",scale=2)




##################################################### FUUUNCTIONS FAIL FAIL FAIL FAIL FAIL
# reorderTable<-function(df,factorCol,valueCol){
#   # get the order of the levels and re-order
#   factorValueOrder <- df[order(-df$valueCol),]$factorCol
#   factorValueOrder <- as.character(factorValueOrder$factorCol)
#   df <- within(factorValueOrder, factorCol <- factor(factorCol, levels = rev(factorValueOrder)))
# }
