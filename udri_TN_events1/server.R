library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

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

## Debrief Data
#system.time(debrief <- sqlQuery(udri, "SELECT * FROM debrief")) # 0.57 seconds
#format(object.size(debrief),units="Mb") # 1.2 megs
system.time(debrief <- sqlQuery(udri, "SELECT Geographic_Location,Serial_Number,Sortie_Date, Sortie_Number,
                                Sortie_Of_Day, Landing_Status, Mission_Code, Takeoff_Date, Takeoff_Time,
                                Landing_Date, Landing_Time, Flight_Duration, Subsystem_Work_Unit_Code,
                                Subsystem_WUC_Description, Work_Unit_Code, WUC_Description, Deviation_Code, 
                                Cause_Code, Deviation_Remarks, Capability_Code, Fault_Code, Job_Control_Number,
                                Discrepancy_Narrative FROM debrief")) # 0.56 seconds
format(object.size(debrief),units="Mb") # 1.0 megs
# add and clean data
debrief <- distinct(debrief)
debrief$Sortie_Date <- ymd(debrief$Sortie_Date, tz='UTC')
debrief$Landing_Date <- ymd(debrief$Landing_Date, tz='UTC')
debrief$Takeoff_Date <- ymd(debrief$Takeoff_Date, tz='UTC')
debrief$Landing_Status <- as.factor(debrief$Landing_Status)
#debrief$Sortie_Modifier <- as.factor(debrief$Sortie_Modifier)
debrief$Capability_Code <- as.factor(debrief$Capability_Code)
debrief$Serial_Number <- as.factor(debrief$Serial_Number)
debrief$Job_Control_Number <- as.factor(debrief$Job_Control_Number)
debrief$Discrepancy_Narrative <- as.character(debrief$Discrepancy_Narrative)
debrief$Deviation_Remarks <- as.character(debrief$Deviation_Remarks)
#debrief$Sortie_DayOfWeek <- wday(debrief$Sortie_Date, label = TRUE)
debrief$Mission_Code <- as.character(debrief$Mission_Code)
debrief$Mission_Code <- gsub(pattern="-",replacement="",x=debrief$Mission_Code)
debrief$Mission_Code <- gsub(pattern=" ",replacement="",x=debrief$Mission_Code)
debrief$Mission_Code <- factor(debrief$Mission_Code)
levels(debrief$Mission_Code)[3] <- "450"
mc <- data.frame("Mission_Code"=unique(as.character(debrief$Mission_Code)),"Mission_Class"=NA)
for (className in c("BONE","DARK","FELON","FIEND","HAWK","PUMA","SLAM","SLAYER")){
  mc[grep(className, mc$Mission_Code),]$Mission_Class <- className
}
mc[grep("TH", mc$Mission_Code),]$Mission_Class <- "THUNDER"; mc[grep("DR", mc$Mission_Code),]$Mission_Class <- "THUNDER"; mc[grep("TR", mc$Mission_Code),]$Mission_Class <- "THUNDER"
mc[grep("FN", mc$Mission_Code),]$Mission_Class <- "FIEND";
mc[!is.na(mc$Mission_Code) & is.na(mc$Mission_Class),]$Mission_Class <- "OTHER"
mc$Mission_Class <- factor(mc$Mission_Class)
debrief<-left_join(debrief,mc,by="Mission_Code")
debrief$Mission_Class<-addNA(debrief$Mission_Class)
# oem data
#system.time(oem <- sqlQuery(udri, "SELECT * FROM on_equipment_maintenance")) # 9.35 seconds
#format(object.size(oem),units="Mb") # 33.6 megs
system.time(oem <- sqlQuery(udri, "SELECT Geographic_Location, Serial_Number, Job_Control_Number,
                            Discrepancy_Narrative, Work_Unit_Code, 
                            Type_Maintenance_Code,
                            Action_Taken_Code, When_Discovered_Code,How_Malfunction_Code,How_Malfunction_Class_Ind,
                            Transaction_Date,Labor_Manhours,WUC_Narrative
                            FROM on_equipment_maintenance")) # 2.98 seconds
format(object.size(oem),units="Mb") # 8 megs

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

# calcualte timeline data
# achieved hours by tail
achievedHrsByTN <- group_by(debrief, Sortie_Number, Sortie_Date, Serial_Number, Mission_Class, Flight_Duration) %>% summarise(achievedSortieHours = max(Flight_Duration)) %>% group_by(Serial_Number, Mission_Class, Sortie_Date) %>% summarise(achievedHours = sum(achievedSortieHours))
achievedHrsByTN <- achievedHrsByTN[order(achievedHrsByTN$Serial_Number, achievedHrsByTN$Sortie_Date),]
achievedHrsByTNaggHr <- group_by(achievedHrsByTN,Serial_Number,Sortie_Date) %>% summarise(dayAchievedHours = sum(achievedHours)) %>% mutate(acrFlHr = cumsum(dayAchievedHours))
achievedHrsByTNaggHr$Sortie_Date <- ymd(achievedHrsByTNaggHr$Sortie_Date)
lifetimes <- expand.grid("Serial_Number"=unique(achievedHrsByTNaggHr$Serial_Number),"Sortie_Date"=ymd((as.Date("2014-05-31 UTC")+seq(61))))
lifetimes <- right_join(achievedHrsByTNaggHr,lifetimes,by=c("Serial_Number","Sortie_Date"))
lifetimes$acrFlHr <- NULL # redo this with zeros
lifetimes[is.na(lifetimes$dayAchievedHours),]$dayAchievedHours <- 0 # set to zero
lifetimes <- group_by(lifetimes, Serial_Number) %>% mutate(acrFlHr = cumsum(dayAchievedHours))
eventsDB <- select(oem, Serial_Number, Transaction_Date, Action_Taken_Code, Work_Unit_Code, WUC_Narrative) %>% filter(Action_Taken_Code %in% c("U","T")) %>% distinct()
eventsDB[,lapply(eventsDB,class)=="factor"] <- lapply(eventsDB[,lapply(eventsDB,class)=="factor"],factor)
eventsDB <- eventsDB[eventsDB$Transaction_Date > '2014-05-31' & eventsDB$Transaction_Date < '2014-08-01',]
maintTNlives <- expand.grid("Serial_Number"=unique(eventsDB[!(eventsDB$Serial_Number %in% lifetimes$Serial_Number),]$Serial_Number),"Sortie_Date"=ymd((as.Date("2014-05-31 UTC")+seq(61))))
maintTNlives$dayAchievedHours <- 0; maintTNlives$acrFlHr <- 0
lifetimes <- rbind(lifetimes,maintTNlives) # join to previous flight hours db
names(lifetimes) <- c("Serial_Number", "Event_Date", names(lifetimes)[3:4])
names(eventsDB) <- c("Serial_Number", "Event_Date", names(eventsDB)[3:5])
lifetimesWevent <- right_join(eventsDB, select(lifetimes, Serial_Number, Event_Date, acrFlHr), by=c("Serial_Number","Event_Date"))


shinyServer(function(input, output) {
  dataSubset <- reactive(
    lifetimesWevent[lifetimesWevent$Serial_Number %in% input$tns 
                    & lifetimesWevent$Event_Date >= as.POSIXct(input$dts[1],origin="1970-01-01") & 
                      lifetimesWevent$Event_Date <= as.POSIXct(input$dts[2],origin="1970-01-01") ,]
  )
  output$dataForDownload <- downloadHandler(
    filename = 'lifetimes.csv',
    content = function(file) {
      write.csv(dataSubset(), file, row.names=FALSE)
    }
  )
  
  output$timelinePlot <- renderPlotly({
    p <- plot_ly(dataSubset(), x=Event_Date,y=acrFlHr, group=Serial_Number,hoverinfo="none") %>%
      layout(xaxis=list(title="Date"),yaxis=list(title="Achieved Flight Hours")) %>%  # had to use two traces to get different colors    
      add_trace(data=dataSubset()[dataSubset()$Action_Taken_Code %in% "U",],x=Event_Date,y=acrFlHr,opacity=0.6,hoverinfo="x+y+text",
                name="Cannibalization<br>Install",mode="markers",marker=list(color="blue", size=10),text=paste0("SN: ",Serial_Number,"<br>Work Unit Code: ",Work_Unit_Code,"<br>",WUC_Narrative)) %>%
      add_trace(data=dataSubset()[dataSubset()$Action_Taken_Code %in% "T",],x=Event_Date,y=acrFlHr,opacity=0.6,hoverinfo="x+y+text",
                name="Cannibalization<br>Removal",mode="markers",marker=list(color="red", size=11),text=paste0("SN: ",Serial_Number,"<br>Work Unit Code: ",Work_Unit_Code,"<br>",WUC_Narrative))
    })
  output$dateRangeText <- renderText(input$dts)
  })