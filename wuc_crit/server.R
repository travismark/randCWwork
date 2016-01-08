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
#ConnectToDB(db.name="udri_demo",user.name="tbaer",pwd="tbaer1",server="onion")
ConnectToDB(db.name="udri_demo",user.name="root",pwd="password",server="localhost")

# Debrief data
print("Load Debrief:")
print(system.time(debrief <- sqlQuery(udri, "SELECT Serial_Number,Sortie_Date, Sortie_Number,
                                Landing_Status, Mission_Code, Takeoff_Date, Takeoff_Time,
                                Landing_Date, Landing_Time, Flight_Duration, Subsystem_Work_Unit_Code,
                                Subsystem_WUC_Description, Work_Unit_Code Debrief_WUC, Deviation_Code, 
                                Cause_Code, Deviation_Remarks, Capability_Code, Job_Control_Number,
                                Discrepancy_Narrative FROM debrief")) )# 0.36 seconds
format(object.size(debrief),units="Mb") # 0.9 megs
debrief <- distinct(debrief)
debrief$Serial_Number <- as.character(debrief$Serial_Number)
debrief$Sortie_Date <- ymd(debrief$Sortie_Date, tz='UTC')
debrief$Landing_Date <- ymd(debrief$Landing_Date, tz='UTC')
debrief$Takeoff_Date <- ymd(debrief$Takeoff_Date, tz='UTC')
debrief$Landing_Status <- as.factor(debrief$Landing_Status)
debrief$Capability_Code <- as.factor(debrief$Capability_Code)
debrief$Job_Control_Number <- as.factor(debrief$Job_Control_Number)
debrief$Discrepancy_Narrative <- as.character(debrief$Discrepancy_Narrative)
debrief$Deviation_Remarks <- as.character(debrief$Deviation_Remarks)
# turn WUC into a factor that matches oem data (take out the ".0" at the end)
debrief$Debrief_WUC <- as.character(debrief$Debrief_WUC)
debrief$Debrief_WUC <- gsub("\\.0","",debrief$Debrief_WUC)
# there are still 3 and 4 digit WUCs.  these will partially match to OEM data
# so don't adjust these
debrief$Debrief_WUC <- factor(debrief$Debrief_WUC)
# turn Subsystem WUC into a factor that matches oem data (take out the ".0" at the end)
debrief$Subsystem_Work_Unit_Code <- as.character(debrief$Subsystem_Work_Unit_Code)
debrief$Subsystem_Work_Unit_Code <- gsub("\\.0","",debrief$Subsystem_Work_Unit_Code)
debrief$Subsystem_Work_Unit_Code <- factor(debrief$Subsystem_Work_Unit_Code)
# add System
debrief$System <- substr(debrief$Subsystem_Work_Unit_Code,1,2)

# WUC data
print("Load WUCs:")
wucSys <- sqlQuery(udri, "SELECT * FROM work_unit_code where length(code)=2")
names(wucSys) <- c("WUC_System","WUC_System_Narrative")
wucSys$WUC_System <- as.character(wucSys$WUC_System)

# OEM data
#system.time(oem <- sqlQuery(udri, "SELECT * FROM on_equipment_maintenance")) # 9.35 seconds
#format(object.size(oem),units="Mb") # 33.6 megs
print("Load OEM")
print(system.time(oem <- sqlQuery(udri, "SELECT Serial_Number, Job_Control_Number,Work_Unit_Code,Action_Taken_Code,
                            Transaction_Date,Labor_Manhours,WUC_Narrative,When_Discovered_Code,Sequence_Number,
                            Type_Maintenance_Code,How_Malfunction_Code,How_Malfunction_Class_Ind, Crew_Size
                            FROM on_equipment_maintenance"))) # 1.50 seconds
format(object.size(oem),units="Mb") # 4.4 megs
oem$Serial_Number <- as.character(oem$Serial_Number)
#oem$How_Malfunction_Code <- as.factor(oem$How_Malfunction_Code)
#oem$How_Malfunction_Class_Ind <- as.factor(oem$How_Malfunction_Class_Ind)
# add date information
oem$Transaction_Date <- ymd(oem$Transaction_Date, tz='UTC')
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
oem$WUC_System <- substr(oem$Work_Unit_Code,1,2)
# fix action taken
oem$Action_Taken_Code <- as.character(oem$Action_Taken_Code)
oem$Action_Taken_Code <- gsub("\\.0","",oem$Action_Taken_Code)
oem$Action_Taken_Code <- factor(oem$Action_Taken_Code)
oem <- left_join(oem,wucSys,by="WUC_System")

# calcualte wuc pareto data
oemWUCDescpareto <- inner_join(oem,debrief,by=c("Serial_Number","Job_Control_Number"))
oemWUCDescpareto <- group_by(oemWUCDescpareto, Work_Unit_Code, WUC_Narrative, WUC_System_Narrative, Serial_Number, Job_Control_Number) %>% 
  summarise(count = n(), lbrManHrs = sum(Labor_Manhours), lbrCalHrs = sum(Labor_Manhours/Crew_Size),
            replaces = sum(Action_Taken_Code %in% 'R'), countAbt = sum(When_Discovered_Code %in% c("A","C")), 
            countAirAbt = sum(When_Discovered_Code %in% "C"), countGndAbt = sum(When_Discovered_Code %in% "A"), 
            fixedBreaks = sum(Action_Taken_Code %in% c("P","R","G","K","L","V","Z") & Landing_Status %in% "3" & Capability_Code %in% c("3","4")), 
            inherantMal = sum(How_Malfunction_Class_Ind == 1,na.rm=TRUE), inducedMal = sum(How_Malfunction_Class_Ind == 2,na.rm=TRUE), 
            noDefectMal = sum(How_Malfunction_Class_Ind == 6,na.rm=TRUE), cnnDup = sum(How_Malfunction_Code %in% c(672,799,812,948) & Action_Taken_Code %in% "H") )
oemWUCDescpareto <- ungroup(oemWUCDescpareto) %>% group_by(Work_Unit_Code, WUC_Narrative,WUC_System_Narrative) %>% 
  summarise(countJCN = n(), countAction = sum(count), lbrManHrs = sum(lbrManHrs), lbrCalHrs = sum(lbrCalHrs),
            replaces = sum(replaces), countAbt = sum(countAbt), 
            countAirAbt = sum(countAirAbt), countGndAbt = sum(countGndAbt), fixedBreaks = sum(fixedBreaks),
            inherantMal = sum(inherantMal), inducedMal = sum(inducedMal), noDefectMal = sum(noDefectMal), cnnDup = sum(cnnDup))
# AWP
# THERE ARE no AWP actions in the merged data - cannibalizations are used to install into an asset that's down after flying
# DEPOT NRTS
# only two of these, a 14KGB and a 23RAD - depotNRTS = sum(Action_Taken_Code %in% c(1,2,3,4,5,6,7,8) | Type_Maintenance_Code %in% "R" | When_Discovered_Code %in% "S")

wucs <- levels(oem$Work_Unit_Code)
gsWucs<-wucs[grep("^0[1-9][0-9A-Za-z]{3}$",wucs)]
oemWUCDescpareto$WUC_Type[oemWUCDescpareto$Work_Unit_Code %in% gsWucs] <- "Support General"
oemWUCDescpareto$WUC_Type[!(oemWUCDescpareto$Work_Unit_Code %in% gsWucs)] <- "Bill of Material"
oemWUCDescpareto$WUC_Type[is.na(oemWUCDescpareto$Work_Unit_Code)] <- "None"
oemWUCDescpareto$WUC_Type <- as.factor(oemWUCDescpareto$WUC_Type)
oemWUCDescpareto$Work_Unit_Code<-as.character(oemWUCDescpareto$Work_Unit_Code);oemWUCDescpareto$WUC_Narrative<-as.character(oemWUCDescpareto$WUC_Narrative)
oemWUCDescpareto$Work_Unit_Code[is.na(oemWUCDescpareto$Work_Unit_Code)]<-"None";oemWUCDescpareto$WUC_Narrative[is.na(oemWUCDescpareto$WUC_Narrative)]<-"No Work Unit Code"
oemWUCDescpareto$Work_Unit_Code<-factor(oemWUCDescpareto$Work_Unit_Code);oemWUCDescpareto$WUC_Narrative<-factor(oemWUCDescpareto$WUC_Narrative)
oemWUCDescpareto$WUC_Narrative <- factor(oemWUCDescpareto$WUC_Narrative)

# working sort properly
oemWUCDescpareto <- oemWUCDescpareto[order(oemWUCDescpareto$lbrManHrs,decreasing=TRUE),]
oemWUCDescparetoOrder <- as.character(oemWUCDescpareto$Work_Unit_Code)
oemWUCDescpareto <- within(oemWUCDescpareto, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(oemWUCDescparetoOrder)))
#oemWUCDescpareto20 <- oemWUCDescpareto[oemWUCDescpareto$Work_Unit_Code %in% head(oemWUCDescparetoOrder,20),]
#oemWUCDescpareto20 <- within(oemWUCDescpareto20, Work_Unit_Code <- factor(Work_Unit_Code, levels = head(oemWUCDescparetoOrder,20)))
#oemWUCDescpareto20 <- oemWUCDescpareto20[order(oemWUCDescpareto20$lbrManHrs),] # resort for plotly
#( ggplot(head(oemWUCDescpareto,20),aes(y=lbrManHrs,x=Work_Unit_Code)) + geom_bar(stat="identity") + coord_flip() )
#oemWUCDescpareto$Work_Unit_Code <- as.character(oemWUCDescpareto$Work_Unit_Code)
#ss <- head(oemWUCDescpareto,20)
#ss$Work_Unit_Code <- factor(ss$Work_Unit_Code)
#ss <- ss[seq(nrow(ss),1),]

####### PLOTLY in case we bail on this app for the dashboard
#( plot_ly(ss,x=lbrManHrs,y=Work_Unit_Code,type="bar",orientation="h")  )
#spotTR<-which(names(oemWUCDescpareto) %in% "replaces")
#names(oemWUCDescpareto)[spotTR] <- "Replaced<br>with Spare"
#( gbpy <- plot_ly(oemWUCDescpareto, y=lbrManHrs,x=countAction,
#                mode="markers", color=`Replaced<br>with Spare`, text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",
#                                            WUC_Narrative,"<br>Type: ",WUC_Type,
#                                            ifelse(WUC_Type %in% "Bill of Material",paste0("<br>System: ",WUC_System_Narrative),""),
#                                            "<br>Spare Replacements: ",`Replaced<br>with Spare`)) %>%
#  layout(yaxis=list(title="Labor ManHours"),xaxis=list(title="Total Actions")) )
#plotly_POST(gbpy, filename="criticality_3axis",world_readable=FALSE,fileopt="overwrite") #  154
# 2axis
#( gbpy <- plot_ly(oemWUCDescpareto, y=lbrManHrs,x=countAction,
#                  mode="markers", text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",
#                                                                              WUC_Narrative,"<br>Type: ",WUC_Type,
#                                                                              ifelse(WUC_Type %in% "Bill of Material",paste0("<br>System: ",WUC_System_Narrative),""))) %>%
#  layout(yaxis=list(title="Labor ManHours"),xaxis=list(title="Total Actions")) )
#plotly_POST(gbpy, filename="criticality_2axis",world_readable=FALSE,fileopt="overwrite") #  156
#names(oemWUCDescpareto)[spotTR] <- "replaces"
# 1 axis
#(gbpy <- plot_ly(oemWUCDescpareto20, y=Work_Unit_Code, x=lbrManHrs, orientation="h", 
#                 type="bar",text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",
#                                        WUC_Narrative,"<br>Type: ",WUC_Type,
#                                        ifelse(WUC_Type %in% "Bill of Material",paste0("<br>System: ",WUC_System_Narrative),""))) %>%
#  layout(yaxis=list(type="category",title="Work Unit Code"),xaxis=list(title="Total Labor Hours"),
#         margin=list("l"=80,"r"=0,"t"=0,"b"=60)) )
#plotly_POST(gbpy, filename="criticality_1axis",world_readable=FALSE,fileopt="overwrite") #  158


metricOptions <- c("Labor ManHours","Labor Calendar Hours","Job Control Numbers","Actions","Replacements With Spare",
                        "Actions Abort","Actions Air Abort","Actions Ground Abort","Break Related Maintenance",
                   "Inherent Failure Actions","Induced Failure Actions","No Defect Actions","Cannot Duplicates")
#metricOptions <- c("Labor ManHours","Job Control Numbers","Actions","Replacements With Spare")
metricCode <- c("lbrManHrs","lbrCalHrs","countJCN","countAction","replaces","countAbt","countAirAbt","countGndAbt","fixedBreaks",
                "inherantMal","inducedMal","noDefectMal","cnnDup")
metricOptCode <- data.frame("english"=metricOptions,"cd"=metricCode)

oemWUCDescpareto <- data.frame(oemWUCDescpareto)

###################### ------------------------ APP
shinyServer(function(input, output) {
  columnNumberPri <- reactive({
    thisCd <- metricOptCode[metricOptCode$english %in% input$mtr1,"cd"]
    columnNumberToReturn <- which(names(oemWUCDescpareto) %in% thisCd)
  })
  columnNumberSec <- reactive({
    thisCd <- metricOptCode[metricOptCode$english %in% input$mtr2,"cd"]
    columnNumberToReturn <- which(names(oemWUCDescpareto) %in% thisCd)
  })
  dataOrder <- reactive({
    oemWUCDescpareto <- oemWUCDescpareto[order(oemWUCDescpareto[,columnNumberPri()],decreasing=TRUE),]
  })
  dataOrderLevels <- reactive({
    oemWUCDescparetoOrder <- dataOrder()$Work_Unit_Code
  })
  dataSubset <- reactive({
    #browser()
    #oemWUCDescpareto$Work_Unit_Code <- as.character(oemWUCDescpareto$Work_Unit_Code)
    oemWUCDescpareto <- dataOrder()
    oemWUCDescparetoTopX <- oemWUCDescpareto[oemWUCDescpareto$Work_Unit_Code %in% dataOrderLevels()[1:input$num],]
    oemWUCDescparetoTopX <- within(oemWUCDescparetoTopX, Work_Unit_Code <- factor(Work_Unit_Code, levels = rev(head(dataOrderLevels(),input$num))))
    # ungroup !
    oemWUCDescparetoTopX# <- data.frame(oemWUCDescparetoTopX)
    # reverse data for plotly
    oemWUCDescparetoTopX[seq(nrow(oemWUCDescparetoTopX),1),]
  })
  metric1 <- reactive(which(names(iris) %in% input$mtr1))
  metric2 <- reactive(which(names(iris) %in% input$mtr2))
  ### OUTPUTS
  output$dataForDownload <- downloadHandler( # not dependent on reactive data
    filename = 'joinedWorkUnitCodeSummary.csv',
    content = function(file) {
      forDL <- oemWUCDescpareto %>% group_by() # change into grouped df to allow spaces in field names
      names(forDL) <- c("Work Unit Code", "WUC Narrative","WUC System Narrative","Job Control Numbers","Actions",
                        "Labor ManHours","Labor Calendar Hours","Replacements With Spare","Actions Abort","Actions Air Abort",
                        "Actions Ground Abort","Break Related Maintenance",
                        "Inherent Failure Actions","Induced Failure Actions","No Defect Actions","Cannot Duplicates",
                        "WUC Type")
      write.csv(oemWUCDescpareto, file, row.names=FALSE)
    }
  )
  output$plot1 <- renderPlotly({
    #browser()
    if(input$oneOrTwo %in% "Pareto") {
    (gbpy <- plot_ly(dataSubset(), y=Work_Unit_Code, x=dataSubset()[,columnNumberPri()], orientation="h", 
                     type="bar",marker=list(color=dataSubset()[,columnNumberSec()]),
                     text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",
                                            WUC_Narrative,"<br>Type: ",WUC_Type,
                                            ifelse(WUC_Type %in% "Bill of Material",paste0("<br>System: ",WUC_System_Narrative),""),
                                 "<br>",input$mtr2,": ",dataSubset()[,columnNumberSec()])) %>%
       layout(yaxis=list(type="category",title="Work Unit Code"),xaxis=list(title=input$mtr1),
              margin=list("l"=80,"r"=0,"t"=0,"b"=40)) )
     # (gbpy <- plot_ly(y=dataSubset()[,"Work_Unit_Code"], x=dataSubset()[,columnNumberPri()], orientation="h", 
    #  (gbpy <- plot_ly(y=dataSubset()[,"Work_Unit_Code"],x=dataSubset()[,"lbrManHrs"], orientation="h", 
    #                   type="bar") %>% #,
                       #text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",
                        #           WUC_Narrative,"<br>Type: ",WUC_Type,
                         #          ifelse(WUC_Type %in% "Bill of Material",paste0("<br>System: ",WUC_System_Narrative),""))) %>%
    #    layout(yaxis=list(type="category",title="Work Unit Code"),xaxis=list(title=input$mtr1)) )
    } else {
      gbpy <- plot_ly(dataSubset(), y=dataSubset()[,columnNumberPri()],x=dataSubset()[,columnNumberSec(),],
                      mode="markers", text=paste0("Work Unit Code: ",Work_Unit_Code,"<br>Narrative: ",
                                                  WUC_Narrative,"<br>Type: ",WUC_Type,
                                                  ifelse(WUC_Type %in% "Bill of Material",paste0("<br>System: ",WUC_System_Narrative),""))) %>%
        layout(yaxis=list(title=input$mtr1),xaxis=list(title=input$mtr2))
    }
#      (gbpy <- plot_ly(iris, y=Sepal.Length, x=iris[,metric1()], mode="markers") )
    
#    (gb <- ggplot(dataSubset(), aes(x=Work_Unit_Code,y=lbrManHrs,fill=WUC_Type)) +
#       geom_bar(stat="identity") + labs(x="Work Unit Code",y="Total Labor Hours, OEM Data",title="Top 50 Work Unit Codes by Labor Hours") + 
#       coord_flip() + scale_fill_manual(values=c("lightblue","grey79","palegreen2")) +
#       geom_text(cex=4,aes(hjust=0,x=Work_Unit_Code,y=0,label=paste(WUC_Narrative))) + theme_bw() +
#       theme(panel.border = element_blank(),axis.line = element_line(color = 'black'),panel.grid.major.y = element_blank()) )
    })
  })

## add third dimension, color, for scatter (or second for pareto)

# turn fixed breaks into two categories - caused abort and didnt/all?

## try to get # of times show up in an abort WUC or Subsystem WUC
# cannibalizations - none
#  NRTS, only two depot
# awp days - none

# BREAK RELATED REPAIRS
# if DEBRIEF LANDINGS STATUS CODE = "3" and (Major discrepancies rendering NMC, may require replacing components)
# a related discrepancy record with capability code equal to "3" or "4" (Performed unsatisfactorily)
# and for each debriefing discrepancy count the number of
# on and off equipment repairs with the same command
# ON EQUIP RECORD WITH
# ACTION TAKEN CODE equals "P," "R," "G," "K," "L," "V," "Z" (remove;replace w/spare;repr/repl minor parts;
#calibrated;adjusted;clean;treat corrosion)

# a fix axis scale option for scatterplot

# removals - all, unscheduled, scheduled (item 70-72)
# condemnations - cannot do b/c only on-equipment