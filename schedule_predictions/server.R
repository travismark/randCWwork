library(shiny);library(plotly);library(dplyr);library(lubridate);library(scales)

ConnectToDB = function(db.name,user.name,pwd,driver.name='MySQL ODBC 5.3 ANSI Driver',server='127.0.0.1',port='3306'){
  con.text <- paste0("DRIVER={",driver.name,"};",
                     "Server=",server,";",
                     "Port=",port,";",
                     "Database=", db.name, ";",
                     "UID=", user.name, ";",
                     "PWD=", pwd)   
  assign("udri",odbcDriverConnect(con.text),envir = .GlobalEnv) 
}
ConnectToDB(db.name="udri_demo",user.name="tbaer",pwd="tbaer1",server="onion")

## Locaiton Data
geoLoc <- sqlQuery(udri, "SELECT * from geoloc_code")
names(geoLoc) <- c("Geographic_Location","Location_Name","City","State")

# wuc system info
wucSys <- sqlQuery(udri, "SELECT * FROM work_unit_code where length(code)=2")
names(wucSys)[1] <- "System"
wucSys$System <- as.character(wucSys$System)

## Debrief Data
#system.time(debrief <- sqlQuery(udri, "SELECT * FROM debrief")) # 0.57 seconds
#format(object.size(debrief),units="Mb") # 1.2 megs
print("Load Debrief:")
print(system.time(debrief <- sqlQuery(udri, "SELECT Geographic_Location,Serial_Number,Sortie_Date, Sortie_Number,
                                Sortie_Of_Day, Landing_Status, Mission_Code, Takeoff_Date, Takeoff_Time,
                                Landing_Date, Landing_Time, Flight_Duration, Subsystem_Work_Unit_Code,
                                Subsystem_WUC_Description, Work_Unit_Code, WUC_Description, Deviation_Code, 
                                Cause_Code, Deviation_Remarks, Capability_Code, Fault_Code, Job_Control_Number,
                                Discrepancy_Narrative FROM debrief")) )# 0.56 seconds
format(object.size(debrief),units="Mb") # 1.0 megs
# add and clean data
debrief <- distinct(debrief)
debrief$Sortie_Date <- ymd(debrief$Sortie_Date, tz='UTC')
debrief$Landing_Date <- ymd(debrief$Landing_Date, tz='UTC')
debrief$Takeoff_Date <- ymd(debrief$Takeoff_Date, tz='UTC')
debrief$Landing_Status <- as.factor(debrief$Landing_Status)
debrief$Capability_Code <- as.factor(debrief$Capability_Code)
debrief$Serial_Number <- as.factor(debrief$Serial_Number)
debrief$Job_Control_Number <- as.factor(debrief$Job_Control_Number)
debrief$Discrepancy_Narrative <- as.character(debrief$Discrepancy_Narrative)
debrief$Deviation_Remarks <- as.character(debrief$Deviation_Remarks)
debrief$Mission_Code <- as.character(debrief$Mission_Code) # fix mission code
debrief$Mission_Code <- gsub(pattern="-",replacement="",x=debrief$Mission_Code)
debrief$Mission_Code <- gsub(pattern=" ",replacement="",x=debrief$Mission_Code)
debrief$Mission_Code <- factor(debrief$Mission_Code)
levels(debrief$Mission_Code)[3] <- "450" # change "450.0" to "450" fix the mission code in debrief
mc <- data.frame("Mission_Code"=unique(as.character(debrief$Mission_Code)),"Mission_Class"=NA,"Mission_Class_2"=NA)
for (className in c("BONE","DARK","FELON","FIEND","HAWK","PUMA","SLAM","SLAYER")){
  mc[grep(className, mc$Mission_Code),]$Mission_Class <- className
}
for (className in c("BONE","DARK","FELON","FIEND","HAWK","PUMA","SLAM","SLAYER","DITTO","FURY","OGRE","LIGHT","RAMBO","RAZOR","SABRE")){
  mc[grep(className, mc$Mission_Code),]$Mission_Class_2 <- className
}
mc[grep("TH", mc$Mission_Code),]$Mission_Class <- "THUNDER"; mc[grep("DR", mc$Mission_Code),]$Mission_Class <- "THUNDER"; mc[grep("TR", mc$Mission_Code),]$Mission_Class <- "THUNDER"
mc[grep("FN", mc$Mission_Code),]$Mission_Class <- "FIEND";
mc[grep("TH", mc$Mission_Code),]$Mission_Class_2 <- "THUNDER"; mc[grep("DR", mc$Mission_Code),]$Mission_Class_2 <- "THUNDER"; mc[grep("TR", mc$Mission_Code),]$Mission_Class_2 <- "THUNDER"
mc[grep("FN", mc$Mission_Code),]$Mission_Class_2 <- "FIEND"; mc[grep("FU", mc$Mission_Code),]$Mission_Class_2 <- "FURY"; mc[grep("O3GZ", mc$Mission_Code),]$Mission_Class_2 <- "OGRE"
mc[!is.na(mc$Mission_Code) & is.na(mc$Mission_Class),]$Mission_Class <- "OTHER"
mc[!is.na(mc$Mission_Code) & is.na(mc$Mission_Class_2),]$Mission_Class_2 <- "OTHER"
mc$Mission_Class <- factor(mc$Mission_Class);mc$Mission_Class_2 <- factor(mc$Mission_Class_2)
debrief<-left_join(debrief,mc,by="Mission_Code")
debrief$Mission_Class<-addNA(debrief$Mission_Class)
debrief$Mission_Class_2<-addNA(debrief$Mission_Class_2)
debrief$System <- substr(debrief$Subsystem_Work_Unit_Code,1,2)

# oem data
#system.time(oem <- sqlQuery(udri, "SELECT * FROM on_equipment_maintenance")) # 9.35 seconds
#format(object.size(oem),units="Mb") # 33.6 megs
print("Load OEM:")
print(system.time(oem <- sqlQuery(udri, "SELECT Geographic_Location, Serial_Number, Job_Control_Number,Work_Unit_Code, 
                            Type_Maintenance_Code,Action_Taken_Code,Transaction_Date,Labor_Manhours,WUC_Narrative
                            FROM on_equipment_maintenance")) )# 2.24 seconds
format(object.size(oem),units="Mb") # 4.2 megs

oem$Serial_Number <- as.factor(oem$Serial_Number)
#oem$Discrepancy_Narrative <- as.character(oem$Discrepancy_Narrative)
#oem$Corrective_Narrative <- as.character(oem$Corrective_Narrative)
#oem$How_Malfunction_Code <- as.factor(oem$How_Malfunction_Code)
#oem$How_Malfunction_Class_Ind <- as.factor(oem$How_Malfunction_Class_Ind)
oem$Transaction_Date <- ymd(oem$Transaction_Date, tz='UTC')
#oem$Transaction_DayOfWeek <- wday(oem$Transaction_Date, label = TRUE)
#oem$Transaction_Month <- as.factor(month(oem$Transaction_Date))
#oem$Transaction_DayOfMonth <- day(oem$Transaction_Date)
oem$Job_Control_Number <- as.character(oem$Job_Control_Number)
oem$Job_Control_Number <- gsub("\\.0","",oem$Job_Control_Number)
oem$Job_Control_Number <- factor(oem$Job_Control_Number)
oem$WUC_Group <- substr(oem$Work_Unit_Code,1,2)

##################
# Sortie Outcomes
# from before : sortieStatuses, but different classifications
predSortie <- group_by(debrief,Serial_Number,Sortie_Number,Sortie_Date,Mission_Class_2,Geographic_Location) %>% summarise(flHr = max(Flight_Duration))
devSortie <- group_by(debrief,Serial_Number,Sortie_Number,Sortie_Date,Mission_Class_2,Deviation_Code) %>% summarise()
jcnSortie <- group_by(debrief,Serial_Number,Sortie_Number,Sortie_Date,Mission_Class_2,Job_Control_Number) %>% filter(!(is.na(Job_Control_Number))) %>% summarise()
jcnSortieSum <- summarise(jcnSortie, countJCN = n())
predSortie <- left_join(predSortie,jcnSortieSum)
predSortie[is.na(predSortie$countJCN),]$countJCN <- 0
# 1) Successful - Perfect # no deviation code, no jcn
devSortieA <- devSortie
devSortieA$Deviation_Bool <- 0
devSortieA[!is.na(devSortieA$Deviation_Code),"Deviation_Bool"]<-1
devSortieA <- summarise(devSortieA, devCodes = sum(Deviation_Bool)) %>% distinct()
predSortie <- inner_join(predSortie,select(devSortieA,Serial_Number,Sortie_Number,Sortie_Date,devCodes))
predSortie$Sortie_Result <- "Other"
predSortie[predSortie$devCodes == 0 & predSortie$countJCN == 0,]$Sortie_Result <- "No Deviation No Maintenance"
#predSortie$devCodes <- NULL
# 2) Successful - Maintenance # no deviation code, with jcn
predSortie[predSortie$devCodes == 0 & predSortie$countJCN > 0,]$Sortie_Result <- "No Deviation With Maintenance"
# 3a) Successful - With Deviation & Maintenance # no abort deviation code, with jcn
devSortieSuc <- devSortie
devSortieSuc$Mission_Success_Int <- 0 # true
devSortieSuc[devSortieSuc$Deviation_Code %in% c("SP","CX","TS","GA","AA","AI"),"Mission_Success_Int"] <- 1 # false
devSortieSuc <- summarise(devSortieSuc, Mission_Success_Int = sum(Mission_Success_Int))
devSortieSuc$Deviation_Code <- NULL
devSortieSuc <- distinct(devSortieSuc)
devSortieSuc$Mission_Success <- FALSE
devSortieSuc[devSortieSuc$Mission_Success_Int == 0,]$Mission_Success <- TRUE
devSortieSuc$Mission_Success_Int <- NULL
predSortie <- left_join(predSortie,devSortieSuc) 
# then update to sortie table - only update those that haven't been assigned (are named "Other")
predSortie[predSortie$Mission_Success & predSortie$countJCN > 0 & predSortie$Sortie_Result %in% "Other",
           "Sortie_Result"] <- "Success but Deviation With Maintenance"
# 3b) same but has no JCN
predSortie[predSortie$Mission_Success & predSortie$countJCN == 0 & predSortie$Sortie_Result %in% "Other",
           "Sortie_Result"] <- "Success but Deviation No Maintenance"
predSortie$Mission_Success <- NULL
# Non-Successful
# 4) Ground Abort
devSortieNonSuc <- devSortie %>% filter(Deviation_Code %in% c("AI","AA","GA","TS","SP")) # no duplicates b/c either or but not both (late landing etc. causes duplicates)
predSortie <- left_join(predSortie,devSortieNonSuc)
predSortie[predSortie$Deviation_Code %in% "GA",]$Sortie_Result <- "Ground Abort"
# 5) Air Abort
predSortie[predSortie$Deviation_Code %in% c("AA","AI"),]$Sortie_Result <- "Air Abort"
# 6) Aircraft Swap (deviation code TS or SP)
predSortie[predSortie$Deviation_Code %in% c("SP","TS"),]$Sortie_Result <- "A/C Swap"
predSortie$Deviation_Code <- NULL
# 7) Cancelled
devSortieNonSuc <- devSortie %>% filter(Deviation_Code %in% c("CX")) # need to add this later b/c adding with others gives a duplicate
predSortie <- left_join(predSortie,devSortieNonSuc)
predSortie[predSortie$Deviation_Code %in% "CX" & predSortie$Sortie_Result %in% "Other",]$Sortie_Result <- "Canceled"
predSortie$Deviation_Code <- NULL
predSortie$Sortie_Result <- factor(predSortie$Sortie_Result)

#### Mission Type Flight Hours Boxplot
sortieHrs <- group_by(debrief, Sortie_Number, Serial_Number, Sortie_Date, Mission_Code, Mission_Class, Mission_Class_2) # includes aborted, cancelled, tail-swap, etc. (sometimes mutliple SNs per sortie)
sortieHrs <- summarise(sortieHrs, maxFlHr = max(Flight_Duration), records = n())
#sortieHrs1MC <- sortieHrs[sortieHrs$Mission_Class_2=="BONE",] # default choice
#sortieHrs1MC$cnt <- nrow(sortieHrs1MC)
sortieHrsB <- sortieHrs
sortieHrsB$cnt <- nrow(sortieHrsB)
sortieHrsB$Mission_Class_2 <- as.character(sortieHrsB$Mission_Class_2)
sortieHrsB$Mission_Class_2 <- "All Types"
#sortieHrsB <- rbind(sortieHrsB,sortieHrs1MC)
#gmcbx <- ggplot(sortieHrsB, aes(y=maxFlHr,x=paste0(Mission_Class_2,"\n",cnt," Sorties"))) + 
#  geom_boxplot() + coord_flip() + theme_bw() + labs(x=NULL,y="Achieved Flight Hours",title="Mission Type Compared to all Sorties")
###

#### System Outcomes (pre - flight)
labor_hrs <- oem %>% group_by(Serial_Number, Job_Control_Number, Work_Unit_Code, WUC_Narrative, WUC_Group, Action_Taken_Code) %>%
  select(Labor_Manhours) %>% summarise(records_in_on_eq_mnt = n(),
                                       total_Labor_Manhours = round(sum(Labor_Manhours, na.rm = TRUE),1)) %>% as.data.frame()

merged_data <-  inner_join(select(debrief,Serial_Number, Job_Control_Number, System, Subsystem_Work_Unit_Code, Subsystem_WUC_Description, Sortie_Date, Sortie_Number, Flight_Duration, Mission_Class_2),
                           labor_hrs,by = c("Job_Control_Number", "Serial_Number"))

#Now I have to group the labor hours within in an individual flight; I don't care about JCN (that was only needed to link the tables)
full_labor_hour_split <- merged_data %>%
  group_by(System, Mission_Class_2, Work_Unit_Code, WUC_Narrative, WUC_Group, Serial_Number, Sortie_Date, Sortie_Number, Flight_Duration) %>%
  select(records_in_on_eq_mnt, total_Labor_Manhours) %>% summarise(
    records_in_on_eq_mnt = sum(records_in_on_eq_mnt), total_Labor_Manhours = round(sum(total_Labor_Manhours, na.rm = TRUE),1)) %>% as.data.frame()

#Create a count of the number of records per grouping in the dataset
counts <- full_labor_hour_split %>% group_by(System, Mission_Class_2, Work_Unit_Code, WUC_Narrative, WUC_Group) %>%
  select(total_Labor_Manhours) %>% summarise(records = n()) %>% as.data.frame()

#Now merge
full_labor_hour_split <-  inner_join(full_labor_hour_split, counts,
                                     by = c("System", "Mission_Class_2", "Work_Unit_Code", "WUC_Narrative", "WUC_Group")) %>% as.data.frame()
rm(counts)

#Now repeat that process, but use a subset of merged data that includes only R Action_Taken_Codes to determine how many of the on_eq_mnt records have an action code of R
full_labor_hour_split_r_only <- merged_data[which(merged_data$Action_Taken_Code=="R"),] %>%
  group_by(System, Mission_Class_2, Work_Unit_Code, WUC_Narrative, WUC_Group, Serial_Number, Sortie_Date, Sortie_Number, Flight_Duration) %>%
  select(records_in_on_eq_mnt, total_Labor_Manhours) %>%
  summarise(r_records_in_on_eq_mnt = sum(records_in_on_eq_mnt)) %>% as.data.frame()

#Now merge so that we can have both the number of records from on_eq_mnt AND the number of those records that had an Action_Code of R.
full_labor_hour_split <-  left_join(full_labor_hour_split, full_labor_hour_split_r_only,
                                    by = c("System", "Mission_Class_2", "Work_Unit_Code", "WUC_Narrative", "WUC_Group", "Serial_Number", "Sortie_Date", "Sortie_Number", "Flight_Duration"))

#Replace all NAs with 0s for the merged file in the r_records field. No idea why it has to be this tough.
full_labor_hour_split[["r_records_in_on_eq_mnt"]][is.na(full_labor_hour_split[["r_records_in_on_eq_mnt"]])] = 0
#full_labor_hour_split$Flight_Duration = round(full_labor_hour_split$Flight_Duration,1)

#Bring in the Sortie Grouping created above
full_labor_hour_split <-  left_join(full_labor_hour_split, ungroup(predSortie) %>% 
                                      select(Sortie_Number,Sortie_Date,Serial_Number,Geographic_Location,countJCN,devCodes,Sortie_Result),
                                    by = c("Serial_Number", "Sortie_Date", "Sortie_Number")) %>% left_join(wucSys) %>% 
  group_by(System, Sortie_Result, Serial_Number, Sortie_Number, Sortie_Date) 
rm(merged_data)

########## SHINY APP BELOW ---------------------------------------------------------

shinyServer(function(input, output) {
  ### REACTIVE DATA
  missionOutcomeTable <- reactive(
    data.frame(prop.table(table(predSortie[predSortie$Mission_Class_2 %in% input$mType,"Sortie_Result"])))
  )
  sortieHrs1MC <- reactive(
    sortieHrs[sortieHrs$Mission_Class_2==input$mType,]
  )
  systemLaborPreF <- reactive(
    full_labor_hour_split[full_labor_hour_split$Mission_Class_2 %in% input$mType,] ### ADD GEO INFO & full_labor_hour_split$Geographic_Location %in% "FNWZ",]
  )
  ### OUTPUTS
  output$missionHoursPlot <- renderPlot({
    sortieHrsA <- sortieHrs1MC()
    sortieHrsA$cnt <- nrow(sortieHrsA)
    sortieHrsB <- rbind(sortieHrsB,sortieHrsA)
    (gmcbx <- ggplot(sortieHrsB, aes(y=maxFlHr,x=paste0(Mission_Class_2,"\n",cnt," Sorties"))) + 
      geom_boxplot() + coord_flip() + theme_bw() + labs(x=NULL,y="Achieved Flight Hours",title="Mission Type Compared to all Sorties"))
      }
    ,height=175)
#  output$tabMisOutSuc <- renderText(ifelse(round(sum(missionOutcomeTable()[5:8,"Freq"]),3)==0,"0%",percent(round(sum(missionOutcomeTable()[5:8,"Freq"]),3))))
#  output$tabMisOutUnsuc <- renderText(ifelse(round(sum(missionOutcomeTable()[1:4,"Freq"]),3)==0,"0%",percent(round(sum(missionOutcomeTable()[1:4,"Freq"]),3))))
  output$missionTypeOutcomes <- renderTable({
    # to cheat the table and get the header bold, make the summary the data frame headers
    completedMissionPercent <- ifelse(round(sum(missionOutcomeTable()[5:8,"Freq"]),3)==0,"0%",percent(round(sum(missionOutcomeTable()[5:8,"Freq"]),3)))
    incompleteMissionPercent <- ifelse(round(sum(missionOutcomeTable()[1:4,"Freq"]),3)==0,"0%",percent(round(sum(missionOutcomeTable()[1:4,"Freq"]),3)))
    # rearrange 
    outcomes<-cbind(missionOutcomeTable()[5:8,],missionOutcomeTable()[1:4,])
    outcomes[,2]<-ifelse(round(outcomes[,2],3)==0,"0%",percent(round(outcomes[,2],3)))
    outcomes[,4]<-ifelse(round(outcomes[,4],3)==0,"0%",percent(round(outcomes[,4],3)))
    names(outcomes) <- c("Successful Sorties",completedMissionPercent,"Unsuccessful Sorties",incompleteMissionPercent)
    outcomes
    },
    include.rownames=FALSE
  )
  output$systemMaintenanceProbabilities <- renderPlot({
    full_labor_hour_split_subset <- systemLaborPreF()
    full_labor_hour_split_subset_Agg <- group_by(full_labor_hour_split_subset,System,description,Sortie_Result) %>% 
      summarise(records_in_on_eq_mnt = sum(records_in_on_eq_mnt), r_records_in_on_eq_mnt = sum(r_records_in_on_eq_mnt), 
                countJCN = sum(countJCN), total_Labor_Manhours = sum(total_Labor_Manhours))
    full_labor_hour_split_subset_Agg$propOfGrp <- full_labor_hour_split_subset_Agg$total_Labor_Manhours/sum(full_labor_hour_split_subset_Agg$total_Labor_Manhours)
    full_labor_hour_split_subset_Agg <- full_labor_hour_split_subset_Agg[order(full_labor_hour_split_subset_Agg$propOfGrp,decreasing=TRUE),]
    full_labor_hour_split_subset_Agg <- head(full_labor_hour_split_subset_Agg,5) # limit to five
    full_labor_hour_split_subset_Sortie <- group_by(full_labor_hour_split_subset,System,description,Sortie_Result,Serial_Number,Sortie_Number,Sortie_Date) %>% 
      filter(System %in% full_labor_hour_split_subset_Agg$System & Sortie_Result %in% full_labor_hour_split_subset_Agg$Sortie_Result) %>%
      summarise(records_in_on_eq_mnt = sum(records_in_on_eq_mnt), r_records_in_on_eq_mnt = sum(r_records_in_on_eq_mnt), 
                countJCN = sum(countJCN), total_Labor_Manhours = sum(total_Labor_Manhours))
    full_labor_hour_split_subset_Sortie <- left_join(full_labor_hour_split_subset_Sortie,select(full_labor_hour_split_subset_Agg,System,Sortie_Result,propOfGrp))
    (ggbx <- ggplot(full_labor_hour_split_subset_Sortie,
                    aes(x=paste("System",System,"\n",description,"\n",Sortie_Result,"\n",paste0(100*round(propOfGrp,3),"%")),
                        y=total_Labor_Manhours)) + geom_boxplot() + coord_flip() + labs(x=NULL,y="Sortie Labor Hours") )
  })
  #output$plannedSorts <- renderTable(data.frame("Serial.Number"=input$))
  #output$tabMisOutSuc <- renderText(100)#   output$timelinePlot <- renderPlotly({
#     p <- plot_ly(dataSubset(), x=Event_Date,y=acrFlHr, group=Serial_Number,hoverinfo="none") %>%
#       layout(xaxis=list(title="Date"),yaxis=list(title="Achieved Flight Hours")) %>%  # had to use two traces to get different colors    
#       add_trace(data=dataSubset()[dataSubset()$Action_Taken_Code %in% "U",],x=Event_Date,y=acrFlHr,opacity=0.6,hoverinfo="x+y+text",
#                 name="Cannibalization<br>Install",mode="markers",marker=list(color="blue", size=10),text=paste0("SN: ",Serial_Number,"<br>Work Unit Code: ",Work_Unit_Code,"<br>",WUC_Narrative)) %>%
#       add_trace(data=dataSubset()[dataSubset()$Action_Taken_Code %in% "T",],x=Event_Date,y=acrFlHr,opacity=0.6,hoverinfo="x+y+text",
#                 name="Cannibalization<br>Removal",mode="markers",marker=list(color="red", size=11),text=paste0("SN: ",Serial_Number,"<br>Work Unit Code: ",Work_Unit_Code,"<br>",WUC_Narrative))
#     })
  }
)