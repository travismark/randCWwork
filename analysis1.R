# University of Dayton Research Institute
# Autumn 2015
require(RODBC);require(lubridate);require(ggplot2);require(tm);require(dplyr);require(tidyr)
udri <- odbcConnect(dsn="onion-udri",uid="tbaer",pw="tbaer1") # created through the Data Sources (ODBC) window described above
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
# now set to factor - only 27 systems
debrief$Discrepancy_Narrative_System<-as.factor(debrief$Discrepancy_Narrative_System)
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
# abortedSorties <- abortedSorties[!is.na(abortedSorties$Mission_Code),]# drop the NA
# merge with average code and class 
  # 1) only use code if more than one sortie, 
  # 2) assign NAs to the median of all missions in avgMisCodeHrs
medMisCodeHrs <- filter(medMisCodeHrs, count>1)
abortedSorties <- left_join(abortedSorties, select(medMisCodeHrs, Mission_Code, medCodeFltDur), by="Mission_Code")
abortedSorties <- left_join(abortedSorties, select(medMisClassHrs, Mission_Class, medClassFltDur), by="Mission_Class")
abortedSorties$diff <- medMisAllHrs # will stay with those sorties with no mission code
abortedSorties[!is.na(abortedSorties$medCodeFltDur),]$diff <- pmax(0,abortedSorties[!is.na(abortedSorties$medCodeFltDur),]$medCodeFltDur-abortedSorties[!is.na(abortedSorties$medCodeFltDur),]$Flight_Duration)
abortedSorties[is.na(abortedSorties$medCodeFltDur),]$diff <- pmax(0,abortedSorties[is.na(abortedSorties$medCodeFltDur),]$medClassFltDur-abortedSorties[is.na(abortedSorties$medCodeFltDur),]$Flight_Duration)

(missingHrs <- sum(abortedSorties$diff))
# by tail number
abortedSortiesByTNsomeAborts <- group_by(abortedSorties, Serial_Number, Mission_Class, Sortie_Date) %>% summarise(missedHours = sum(diff))
missedHoursByTN <- data.frame("Serial_Number"=levels(abortedSortiesByTNsomeAborts$Serial_Number))
missedHoursByTN <- left_join(missedHoursByTN, abortedSortiesByTNsomeAborts, by="Serial_Number")
missedHoursByTN[is.na(missedHoursByTN$missedHours),"missedHours"]<-0 # replace NAs with 0
(ggplot(missedHoursByTN, aes(x=Serial_Number, y=missedHours)) + geom_bar(stat="identity") + coord_flip()) + ylab("Missed Flight Hours due to Aborts")
# could color bars by air/ground, facet for two geographic locations, etc.

# how many achieved hours?
sortieHrs <- group_by(debrief, Sortie_Number, Serial_Number, Sortie_Date, Sortie_DayOfWeek, Mission_Code, Mission_Class) # includes aborted, cancelled, tail-swap, etc. (sometimes mutliple SNs per sortie)
sortieHrs <- summarise(sortieHrs, maxFlHr = max(Flight_Duration), records = n())
(achievedHrs <- sum(sortieHrs$maxFlHr) )

# missed / (missed+achieved) hours
missingHrs / (missingHrs + achievedHrs) # unavailability

# achieved hours by tail
achievedHrsByTN <- group_by(debrief, Sortie_Number, Sortie_Date, Serial_Number, Mission_Class, Flight_Duration) %>% summarise(achievedSortieHours = max(Flight_Duration)) %>% group_by(Serial_Number, Mission_Class, Sortie_Date) %>% summarise(achievedHours = sum(achievedSortieHours))
flightHrsByTN <- full_join(achievedHrsByTN,missedHoursByTN,by=c("Serial_Number","Mission_Class","Sortie_Date"))
# flip the data
flightHrsByTN <- gather(flightHrsByTN,"flightType","flightHours",achievedHours:missedHours)
(gtn <- ggplot(flightHrsByTN, aes(x=Serial_Number,y=flightHours)) + geom_bar(stat="identity", aes(fill=flightType)) + coord_flip() + labs(y="Flight Hours") + ggtitle("Flight Hours by Tail Number, Achieved or Missed"))
ggsave(filename="tail_number_flight_hours_byMissedAchieved.svg", plot=gtn, width=14, height=10, scale=1)
(gtn <- ggplot(flightHrsByTN, aes(x=Serial_Number,y=flightHours)) + geom_bar(stat="identity", aes(fill=Mission_Class)) + coord_flip() + labs(y="Flight Hours") + ggtitle("Flight Hours by Tail Number, Achieved or Missed"))

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

# tail number lifetime
achievedHrsByTN <- achievedHrsByTN[order(achievedHrsByTN$Serial_Number, achievedHrsByTN$Sortie_Date),]
achievedHrsByTNaggHr <- group_by(achievedHrsByTN,Serial_Number) %>% mutate(acrFlHr = cumsum(achievedHours))
(gps <- ggplot(achievedHrsByTNaggHr, aes(x=Sortie_Date,y=acrFlHr)) + geom_line() + facet_wrap(~Serial_Number) + ggtitle("Accrued Flight Hours by Tail Number"))
ggsave(filename="tail_number_accrued_flight_hours_byDate_grid.svg", plot=gps, width=20, height=12, scale=1)
(gps <- ggplot(achievedHrsByTNaggHr, aes(x=Sortie_Date,y=acrFlHr, colour=Serial_Number)) + geom_line(size=1) + ggtitle("Accrued Flight Hours by Tail Number"))
ggsave(filename="tail_number_accrued_flight_hours_byDate_onePlot.svg", plot=gps, width=20, height=12, scale=1)


########### gg plot adds values from rows with same factor - however, the order
abc <- data.frame("c1" = c("a","a","b","b"), "c2"=c(1,2,3,1), "c3"=c("c","d","c","d")) # TIME ZONES ruining this
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

# date histogram
qplot(Transaction_Date, data = oem, fill=Geographic_Location)
qplot(Transaction_Date, data = oem[oem$Transaction_Date >= '2014-01-01' & oem$Transaction_Date < '2015-01-01',], fill=Geographic_Location)

# weird fields
oem[which(oem$On_Maint_Action_Key==max(oem$On_Maint_Action_Key)),"Transaction_Date"]
qplot(y=oem$On_Work_Order_Key, x=oem$Transaction_Date)
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
oemWUCDescpareto <- group_by(oem, WUC_Narrative) %>% filter(!is.na(On_Component_Part_Number)) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours))
#compareA <- oemWUCDescpareto <- group_by(oem, WUC_Narrative) %>% filter(!is.na(On_Component_Part_Number))
#compareB <- inner_join(select(oem,On_Component_Part_Number, Labor_Manhours,Job_Control_Number, Work_Unit_Code, WUC_Narrative), 
 #                            select(debrief, Job_Control_Number, Deviation_Code), by="Job_Control_Number") %>% filter(!is.na(On_Component_Part_Number))
## very different
# filter out Special Purpose WUCs instead of null part numbers
#specialPurposeWUCs <- c(01000,02000,03000,03100,03101,03102,03107,03111,03112,03113,03114,03115,'0311K','0311L','0311M','0311N','0311P','0311R','0311S','0311T','0311U',03121,03128,03130,03142,03156,03184,03200,03205,03209,03210,03212,03215,03220,03221,03268,03300,03305,03310,03311,03312,03313,03314,03320,03330,03336,03340,03360,03370,03380,03390,03395,03400,'0341A','0341B','0341C','0341D','0341E','0341F','0341G','0341H','0341J','0341K','0341L','0341M','0341N','0341P','0341Q','0341R','0341S','0341T','0341U','0341V','0341W','0341X','0341Y','0341Z','0342A','0342B',03510,03580,03596,03597,03600,03610,03700,03710,03711,03712,03713,03714,03720,03721,03722,03723,03724,03730,03731,03732,03750,03755,03800,03802,03803,03804,03806,03900,03999,04000,04100,04101,04110,04111,04112,04113,04114,04115,04116,04117,04118,04119,'0411A','0411B','0411C','0411D','0411E','0411H','0411J','0411K',04120,04121,04122,04123,04124,04125,04126,04127,04128,04129,'0412A','0412B','0412C','0412D','0412E','0412F','0412G','0412H','0412J','0412H','0412J','0412L','0412M','0412N','0412P','0412Q',04130,04131,04132,04133,04134,04135,04136,04137,04138,04139,'0413A','0413B','0413C','0413E','0413F','0413H','0413J','0413K','0413L','0413M','0413N','0413P',04140,04141,04142,04143,04144,04145,04146,04147,04148,04149,04150,04151,04152,'0415A','0415B','0415C',04160,04161,04162,04163,04170,04180,04181,04182,04184,04185,04186,04187,04188,04189,'0418A','0418B','0418C','0418D','0418E','0418F',04190,04199,04200,04210,04220,04221,04222,04227,04228,04270,04280,04310,04311,04313,04314,04315,04316,04317,04320,04321,04322,04324,04325,04326,04327,04330,04340,04341,04342,04343,04344,04345,04346,04347,04348,04349,04350,04351,04352,04353,04354,04355,04356,04358,04359,04360,04361,04362,04363,04364,04365,04366,04367,04370,04371,04372,04373,04400,04500,04510,04572,04573,04574,04575,04576,04577,04578,04583,04584,04610,04620,04630,04650,04660,04999,'04MD4',05000,06000,07000,08000,09000)
supportGeneralWUCs <- c('1000.0','1100.0','2000.0','3000.0','3100.0','3101.0','3102.0','3107.0','3111.0','3112.0','3113.0','3114.0','3115.0', '0311K', '0311L', '0311M', '0311N', '0311P', '0311R', '0311S', '0311T', '0311U','3121.0','3128.0','3130.0','3142.0','3156.0','3184.0','3200.0','3205.0','3209.0','3210.0','3212.0','3215.0','3220.0','3221.0','3268.0','3300.0','3305.0','3310.0','3311.0','3312.0','3313.0','3314.0','3320.0','3330.0','3336.0','3340.0','3360.0','3370.0','3380.0','3390.0','3395.0','3400.0', '0341A', '0341B', '0341C', '0341D', '0341E', '0341F', '0341G', '0341H', '0341J', '0341K', '0341L', '0341M', '0341N', '0341P', '0341Q', '0341R', '0341S', '0341T', '0341U', '0341V', '0341W', '0341X', '0341Y', '0341Z', '0342A', '0342B','3510.0','3580.0','3596.0','3597.0','3600.0','3610.0','3700.0','3710.0','3711.0','3712.0','3713.0','3714.0','3720.0','3721.0','3722.0','3723.0','3724.0','3730.0','3731.0','3732.0','3750.0','3755.0','3800.0','3802.0','3803.0','3804.0','3806.0','3900.0','3999.0','4000.0','4100.0','4101.0','4110.0','4111.0','4112.0','4113.0','4114.0','4115.0','4116.0','4117.0','4118.0','4119.0', '0411A', '0411B', '0411C', '0411D', '0411E', '0411H', '0411J', '0411K','4120.0','4121.0','4122.0','4123.0','4124.0','4125.0','4126.0','4127.0','4128.0','4129.0', '0412A', '0412B', '0412C', '0412D', '0412E', '0412F', '0412G', '0412H', '0412J', '0412H', '0412J', '0412L', '0412M', '0412N', '0412P', '0412Q','4130.0','4131.0','4132.0','4133.0','4134.0','4135.0','4136.0','4137.0','4138.0','4139.0', '0413A', '0413B', '0413C', '0413E', '0413F', '0413H', '0413J', '0413K', '0413L', '0413M', '0413N', '0413P','4140.0','4141.0','4142.0','4143.0','4144.0','4145.0','4146.0','4147.0','4148.0','4149.0','4150.0','4151.0','4152.0', '0415A', '0415B', '0415C','4160.0','4161.0','4162.0','4163.0','4170.0','4180.0','4181.0','4182.0','4184.0','4185.0','4186.0','4187.0','4188.0','4189.0', '0418A', '0418B', '0418C', '0418D', '0418E', '0418F','4190.0','4199.0','4200.0','4210.0','4220.0','4221.0','4222.0','4227.0','4228.0','4270.0','4280.0','4310.0','4311.0','4313.0','4314.0','4315.0','4316.0','4317.0','4320.0','4321.0','4322.0','4324.0','4325.0','4326.0','4327.0','4330.0','4340.0','4341.0','4342.0','4343.0','4344.0','4345.0','4346.0','4347.0','4348.0','4349.0','4350.0','4351.0','4352.0','4353.0','4354.0','4355.0','4356.0','4358.0','4359.0','4360.0','4361.0','4362.0','4363.0','4364.0','4365.0','4366.0','4367.0','4370.0','4371.0','4372.0','4373.0','4400.0','4500.0','4510.0','4572.0','4573.0','4574.0','4575.0','4576.0','4577.0','4578.0','4583.0','4584.0','4610.0','4620.0','4630.0','4650.0','4660.0','4999.0', '04MD4','5000.0','6000.0','7000.0','8000.0','9000.0')
wucs <- levels(oem$Work_Unit_Code)
gsWucs<-wucs[grep("^0[1-9][0-9A-Za-z]{3}$",wucs)]
supportGeneralWUCs <- c(supportGeneralWUCs,gsWucs)
oemWUCDescpareto <- group_by(oem, Work_Unit_Code, WUC_Narrative) %>% filter(!(Work_Unit_Code %in% supportGeneralWUCs)) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours))
(gWucCnLb <- ggplot(oemWUCDescpareto, aes(x=count,y=lbrHrs)) + labs(x="Maintenance Actions",y="Labor Manhours",title="Work Unit Code Comparison") +
  geom_point(aes(text=paste("WUC:",Work_Unit_Code,"Narrative:",WUC_Narrative))))
ggsave(filename="wuc_scatter_notSupGen.svg", plot=gWucCnLb, width=15, height=9,scale=1)

##### focus on all records in OEM first
# relevel
oemWUCDescpareto$WUC_Narrative <- factor(oemWUCDescpareto$WUC_Narrative)
# 1) labor hours
# get the order of the levels and re-order
oemWUCDescparetoOrder <- oemWUCDescpareto[order(-oemWUCDescpareto$lbrHrs),"WUC_Narrative"]
oemWUCDescparetoOrder <- as.character(oemWUCDescparetoOrder$WUC_Narrative)
oemWUCDescpareto <- within(oemWUCDescpareto, WUC_Narrative <- factor(
  WUC_Narrative, levels = rev(oemWUCDescparetoOrder)))
# now plot pareto
gb <- ggplot(oemWUCDescpareto, aes(x=WUC_Narrative,y=lbrHrs)) +
  geom_bar(stat="identity") + labs(y="Total Labor Hours, OEM Data") + coord_flip()
gb
ggsave(filename="wuc_labor_hours_pareto_oem_all.svg", plot=gb, scale=5)
# top twenty
oemWUCDescparetoOrder20 <- head(oemWUCDescparetoOrder, 20)
oemWUCDescpareto20 <- oemWUCDescpareto[oemWUCDescpareto$WUC_Narrative %in% tail(levels(oemWUCDescpareto$WUC_Narrative),20), ]
oemWUCDescpareto20 <- within(oemWUCDescpareto20, WUC_Narrative <- factor(
  WUC_Narrative, levels = rev(oemWUCDescparetoOrder20)))
gb20 <- ggplot(oemWUCDescpareto20, aes(x=WUC_Narrative,y=lbrHrs)) +
  geom_bar(stat="identity") + labs(y="Total Labor Hours, OEM Data") + coord_flip()
gb20
ggsave(filename="wuc_labor_hours_pareto_oem_top20.svg", plot=gb20, width=15, height=9, scale=1)
# 2) Occurances
oemWUCDescparetoOrderAct <- oemWUCDescpareto[order(-oemWUCDescpareto$count),"WUC_Narrative"]
oemWUCDescparetoOrderAct <- as.character(oemWUCDescparetoOrderAct$WUC_Narrative)
oemWUCDescparetoAct <- within(oemWUCDescpareto, WUC_Narrative <- factor(
  WUC_Narrative, levels = rev(oemWUCDescparetoOrderAct)))
gb <- ggplot(oemWUCDescparetoAct, aes(x=WUC_Narrative,y=count)) +
  geom_bar(stat="identity") + labs(y="Total Actions, OEM Data") + coord_flip()
gb
ggsave(filename="wuc_actions_pareto_oem_all.svg", plot=gb, scale=9)
# top twenty
oemWUCDescparetoOrderAct20 <- head(oemWUCDescparetoOrderAct, 20)
oemWUCDescpareto20Act <- oemWUCDescparetoAct[oemWUCDescparetoAct$WUC_Narrative %in% tail(levels(oemWUCDescparetoAct$WUC_Narrative),20), ]
oemWUCDescpareto20Act <- within(oemWUCDescpareto20Act, WUC_Narrative <- factor(
  WUC_Narrative, levels = rev(oemWUCDescparetoOrderAct20)))
gb20 <- ggplot(oemWUCDescpareto20Act, aes(x=WUC_Narrative,y=count)) +
  geom_bar(stat="identity") + labs(y="Total Actions, OEM Data") + coord_flip()
gb20
ggsave(filename="wuc_actions_pareto_oem_top20.svg", plot=gb20, width=15, height=9,scale=1)
# 3) repair days
# distributions? total per week? average?
# total first:
oemWUCDescRepDayspareto <- group_by(oem, WUC_Narrative, Job_Control_Number) %>% filter(!is.na(On_Component_Part_Number)) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours), minDt = min(Transaction_Date), maxDt = max(Transaction_Date))
# add a day to indicate any portion of day maintenance is being performed
# what is this? days between first and last date for this WUC and for this JCN?
oemWUCDescRepDayspareto$maintDays <- 1+difftime(oemWUCDescRepDayspareto$maxDt,oemWUCDescRepDayspareto$minDt,units="days")
oemWUCDescRepDayspareto <- select(oemWUCDescRepDayspareto,WUC_Narrative, maintDays) %>% group_by(WUC_Narrative) %>% summarise(totalMaintDays = sum(maintDays))
# reorder & pareto
theOrder <- oemWUCDescRepDayspareto[order(-as.numeric(oemWUCDescRepDayspareto$totalMaintDays)),"WUC_Narrative"]
theOrder <- as.character(theOrder$WUC_Narrative)
oemWUCDescRepDayspareto <- within(oemWUCDescRepDayspareto, WUC_Narrative <- factor(WUC_Narrative, levels = rev(theOrder)))
gb <- ggplot(oemWUCDescRepDayspareto, aes(x=WUC_Narrative,y=as.numeric(totalMaintDays))) +
  geom_bar(stat="identity") + labs(y="Total Maintenance Days, OEM Data") + coord_flip()
gb
ggsave(filename="wuc_maint_days_pareto_oem_all.svg", plot=gb, width=15, height=40, scale=1)
# top 20
oemWUCDescRepDayspareto20order <- tail(levels(oemWUCDescRepDayspareto$WUC_Narrative), 20)
oemWUCDescRepDayspareto20 <- oemWUCDescRepDayspareto[oemWUCDescRepDayspareto$WUC_Narrative %in% oemWUCDescRepDayspareto20order,]
gb <- ggplot(oemWUCDescRepDayspareto20, aes(x=WUC_Narrative,y=as.numeric(totalMaintDays))) +
  geom_bar(stat="identity") + labs(y="Total Maintenance Days, OEM Data") + coord_flip()
gb
ggsave(filename="wuc_maint_days_pareto_oem_top20.svg", plot=gb, width=10, height=8, scale=1)
### NOW WITH type-maintenance for scheduled and unscheduled - most jcn/wuc combos have only one type, BUT there's still some weird data, like 'radome, tail cone' comes up with a lot more days than before and 'actr, owf l&r' a lot fewer
oemWUCDescRepDaysparetoMType <- group_by(oem, WUC_Narrative, Job_Control_Number, Type_Maintenance_Code) %>% filter(!is.na(On_Component_Part_Number)) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours), minDt = min(Transaction_Date), maxDt = max(Transaction_Date))
oemWUCDescRepDaysparetoMType$maintDays <- 1+difftime(oemWUCDescRepDaysparetoMType$maxDt,oemWUCDescRepDaysparetoMType$minDt,units="days")
oemWUCDescRepDaysparetoMType$maintType <- NA
oemWUCDescRepDaysparetoMType[oemWUCDescRepDaysparetoMType$Type_Maintenance_Code %in% c("A", "C", "D", "E", "H", "J", "P", "Q", "R"),]$maintType <- "Scheduled"
oemWUCDescRepDaysparetoMType[oemWUCDescRepDaysparetoMType$Type_Maintenance_Code %in% c("B", "S", "Y"),]$maintType <- "Unscheduled"
oemWUCDescRepDaysparetoMType$maintType<-factor(oemWUCDescRepDaysparetoMType$maintType)
oemWUCDescRepDaysparetoMType <- select(oemWUCDescRepDaysparetoMType,WUC_Narrative, maintType, maintDays) %>% group_by(WUC_Narrative, maintType) %>% summarise(totalMaintDays = sum(maintDays))
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

############ Now mix with debrief data (by JCN)
# mix oem with debrief for deviation code
# 2b) only for aborts
oemWUCDescparetoDC <- inner_join(select(oem,On_Component_Part_Number, Labor_Manhours,Job_Control_Number, Work_Unit_Code, WUC_Narrative, Transaction_Date, How_Malfunction_Class_Ind, On_Component_Part_Number), 
                                 select(debrief, Job_Control_Number, Deviation_Code, Subsystem_WUC_Description), by="Job_Control_Number")
oemWUCDescparetoDC <- group_by(oemWUCDescparetoDC, WUC_Narrative, Deviation_Code, Subsystem_WUC_Description, Transaction_Date, How_Malfunction_Class_Ind, On_Component_Part_Number) %>% filter(!is.na(On_Component_Part_Number)) %>% summarise(count = n(), lbrHrs = sum(Labor_Manhours))
# find order & plot
oemWUCDescparetoDC$Deviation_Code <- factor(oemWUCDescparetoDC$Deviation_Code);oemWUCDescparetoDC$Subsystem_WUC_Description <- factor(oemWUCDescparetoDC$Subsystem_WUC_Description);oemWUCDescparetoDC$On_Component_Part_Number <- factor(oemWUCDescparetoDC$On_Component_Part_Number);oemWUCDescparetoDC$How_Malfunction_Class_Ind <- factor(oemWUCDescparetoDC$How_Malfunction_Class_Ind)
oemWUCDescparetoDC$WUC_Narrative <- factor(oemWUCDescparetoDC$WUC_Narrative) # relelve the factors
oemWUCDescparetoDCagg <- group_by(oemWUCDescparetoDC, WUC_Narrative) %>% summarise(count = sum(count), lbrHrs = sum(lbrHrs))
oemWUCDescparetoDC <- within(oemWUCDescparetoDC, WUC_Narrative <- factor(WUC_Narrative, levels = levels(oemWUCDescparetoDC$WUC_Narrative)[order(as.numeric(oemWUCDescparetoDCagg$count))])) # might be able to use the reorder() function
( gb <- ggplot(oemWUCDescparetoDC, aes(x=WUC_Narrative, y=count)) + 
  geom_bar(stat="identity", aes(fill=Deviation_Code)) + labs(y="Total Maintenance Actions by WUC & Deviation Code, Joined Data") + coord_flip() )
ggsave(filename="wuc_maint_actions_byType_DevCode_pareto_joined_all.svg", plot=gb, width=15, height=40, scale=1)
# top 20
oemWUCDescparetoDC20 <- oemWUCDescparetoDC[oemWUCDescparetoDC$WUC_Narrative %in% tail(levels(oemWUCDescparetoDC$WUC_Narrative),20),]
( gb <- ggplot(oemWUCDescparetoDC20, aes(x=WUC_Narrative, y=count)) + 
    geom_bar(stat="identity", aes(fill=Deviation_Code)) + labs(y="Total Maintenance Actions by WUC & Deviation Code, Joined Data") + coord_flip() )
ggsave(filename="wuc_maint_actions_byType_DevCode_pareto_joined_top20.svg", plot=gb, width=10, height=8, scale=1)
# by How Malfunction Class
oemWUCDescparetoDC<-oemWUCDescparetoDC[order(oemWUCDescparetoDC$How_Malfunction_Class_Ind),] # so that all HWC are together within each bar
( gb <- ggplot(oemWUCDescparetoDC, aes(x=WUC_Narrative, y=count)) + ggtitle("Total Maintenance Actions by WUC & How-Malfunction Class, Joined Data") +
    geom_bar(stat="identity", aes(fill=How_Malfunction_Class_Ind)) + labs(y="Total Maintenance Actions") + scale_fill_discrete(labels=c("1 - Inherent","2 - Induced","6 - No Defect")) + coord_flip() )
ggsave(filename="wuc_maint_actions_byHowMalClass_pareto_joined_all.svg", plot=gb, width=17, height=40, scale=1)
# top 20
( gb <- ggplot(oemWUCDescparetoDC20, aes(x=WUC_Narrative, y=count)) + ggtitle("Total Maintenance Actions by WUC & How-Malfunction Class, Joined Data") +
    geom_bar(stat="identity", aes(fill=How_Malfunction_Class_Ind)) + labs(y="Total Maintenance Actions") + scale_fill_discrete(labels=c("1 - Inherent","2 - Induced","6 - No Defect")) + coord_flip() )
ggsave(filename="wuc_maint_actions_byHowMalClass_pareto_joined_top20.svg", plot=gb, width=10, height=8, scale=1)

## only aborted ones
oemWUCDescparetoDCabort <- filter(oemWUCDescparetoDC, Deviation_Code %in% c('GA','AI','AA'))
oemWUCDescparetoDCabort$abortType <- "Air"
oemWUCDescparetoDCabort[oemWUCDescparetoDCabort$Deviation_Code %in% 'GA',]$abortType <- "Ground"
# sort levels
oemWUCDescparetoDCabort$Deviation_Code <- factor(oemWUCDescparetoDCabort$Deviation_Code);oemWUCDescparetoDCabort$Subsystem_WUC_Description <- factor(oemWUCDescparetoDCabort$Subsystem_WUC_Description)
oemWUCDescparetoDCabort$WUC_Narrative <- factor(oemWUCDescparetoDCabort$WUC_Narrative) # filter out non-existant WUCs
oemWUCDescparetoDCabortagg <- group_by(oemWUCDescparetoDCabort, WUC_Narrative) %>% summarise(count = sum(count), lbrHrs = sum(lbrHrs))
oemWUCDescparetoDCabort <- within(oemWUCDescparetoDCabort, WUC_Narrative <- factor(WUC_Narrative, levels = levels(oemWUCDescparetoDCabort$WUC_Narrative)[order(as.numeric(oemWUCDescparetoDCabortagg$count))]))
( gb <- ggplot(oemWUCDescparetoDCabort, aes(x=WUC_Narrative, y=count)) + ggtitle("Total Maintenance Actions from Aborted Flights by WUC and Abort Type, Joined Data") + 
    geom_bar(stat="identity", aes(fill=abortType)) + labs(y="Maintenance Actions from Aborted Flights, Joined Data",fill="Abort Type") + coord_flip() )
ggsave(filename="wuc_maint_actions_AbortType_pareto_joined_all.svg", plot=gb, width=11, height=8, scale=1) 
## Connection between debrief subsystem and eventual WUC - way too many of these subystems
( gb <- ggplot(oemWUCDescparetoDC, aes(x=WUC_Narrative, y=count)) + ggtitle("Total Maintenance Actions by WUC & Debrief Subsystem, Joined Data") +
    geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Maintenance Actions") + coord_flip() )
ggsave(filename="wuc_maint_actions_debSubSys_pareto_joined_all.svg", plot=gb, width=20, height=40, scale=1)
# connection of just aborted ones
( gb <- ggplot(oemWUCDescparetoDCabort, aes(x=WUC_Narrative, y=count)) + ggtitle("Maintenance Actions from Aborted Sorties by WUC & Debrief Subsystem, Joined Data") +
    geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Maintenance Actions") + coord_flip() )
ggsave(filename="wuc_maint_actions_aborted_DebSubsys_pareto_joined_all.svg", plot=gb, width=11, height=8, scale=1) 
# facet
( gb <- ggplot(oemWUCDescparetoDCabort, aes(x=WUC_Narrative, y=count)) + ggtitle("Maintenance Actions from Aborted Sorties by WUC, Abort Type, Debrief Subsystem, Joined Data") +
    geom_bar(stat="identity", aes(fill=Subsystem_WUC_Description)) + labs(y="Maintenance Actions") + coord_flip() + facet_grid( . ~ abortType) )
ggsave(filename="wuc_maint_actions_AbortType_DebSubsys_pareto_joined_all.svg", plot=gb, width=18, height=8, scale=1) 

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
awpDates <- group_by(oem, Serial_Number, Job_Control_Number, Action_Taken_Code, Transaction_Date, Work_Unit_Code, WUC_Narrative) %>% filter(Action_Taken_Code %in% c('T','U')) %>% select(Serial_Number, Job_Control_Number, Action_Taken_Code, Transaction_Date, Work_Unit_Code, WUC_Narrative)
awpDates <- awpDates[order(awpDates$Job_Control_Number, awpDates$Transaction_Date),]


######### attribute maintenance hours to particular sorties
sortieHrs[1,] # has flown hours of each sortie (maybe zero)
sortieMaint <- group_by(oem, Job_Control_Number) %>% summarise(jobLaborHrs = sum(Labor_Manhours))
sortieMaint <- select(debrief, Job_Control_Number, Sortie_Number, Sortie_Date, Serial_Number, Deviation_Code) %>% inner_join(sortieMaint, by="Job_Control_Number") %>% group_by(Sortie_Number, Sortie_Date, Serial_Number, Deviation_Code) %>% summarise(sortieLaborHrs = sum(jobLaborHrs))
sortieHrsAndMaint <- full_join(sortieHrs, sortieMaint, by=c("Sortie_Number", "Serial_Number", "Sortie_Date"))
# set NAs to zero
sortieHrsAndMaint[is.na(sortieHrsAndMaint$sortieLaborHrs),]$sortieLaborHrs <- 0
(gd <- ggplot(sortieHrsAndMaint, aes(x=maxFlHr, y=sortieLaborHrs, colour=Mission_Class)) + geom_point(size=2.5) + labs(x="Sortie Flight Hours",y="Labor Hours From Sortie",title="Flight and Labor Hours Per Attempted Sortie"))
ggsave(filename="sortie_flightHrs_laborHrs_byMisClass.svg", plot=gd, width=15,height=12)
(gd <- ggplot(sortieHrsAndMaint, aes(x=maxFlHr, y=sortieLaborHrs, colour=Deviation_Code)) + geom_point(size=2.5) + labs(x="Sortie Flight Hours",y="Labor Hours From Sortie",title="Flight and Labor Hours Per Attempted Sortie"))
ggsave(filename="sortie_flightHrs_laborHrs_byDevCd.svg", plot=gd, width=15,height=12)

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
