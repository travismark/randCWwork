library(shiny);library(plotly);library(RODBC);library(scales)

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
#ConnectToDB(db.name="udri_demo",user.name="root",pwd="password",server="localhost")

## Location Data
#geoLoc <- sqlQuery(udri, "SELECT * from geoloc_code")
#names(geoLoc) <- c("Geographic_Location","Location_Name","City","State")

# udri <- odbcConnect(dsn="onion-udri",uid="tbaer",pw="tbaer1") # created through the Data Sources (ODBC) window described above
# oem data
#debDev <- sqlQuery(udri, "SELECT * FROM deviation_code")
#debSubsys <- sqlQuery(udri, "SELECT distinct Subsystem_Work_Unit_Code, subsystem_wuc_description FROM debrief")
#debSubsys$Subsystem_Work_Unit_Code <- as.character(debSubsys$Subsystem_Work_Unit_Code)
#debSubsys$Subsystem_Work_Unit_Code[is.na(debSubsys$Subsystem_Work_Unit_Code)] <- "NONE"
#debSubsys$Subsystem_Work_Unit_Code <- gsub("\\.0","",debSubsys$Subsystem_Work_Unit_Code)
#debSys <- sqlQuery(udri, "SELECT * FROM work_unit_code where length(code) = 2")
debSys <- sqlQuery(udri, "SELECT DISTINCT wuc.* FROM work_unit_code wuc JOIN debrief d ON wuc.code = LEFT(d.work_unit_code,2) ORDER BY code")
# oemWuc <- sqlQuery(udri, "SELECT distinct Work_Unit_Code, Job_Control_Number, Serial_Number FROM on_equipment_maintenance")
# oemWuc$Serial_Number <- as.character(oemWuc$Serial_Number)
# oemWuc$Job_Control_Number <- as.character(oemWuc$Job_Control_Number)
# oemWuc$Job_Control_Number <- gsub("\\.0","",oemWuc$Job_Control_Number)
# debWuc <- sqlQuery(udri, "SELECT distinct Serial_Number, Job_Control_Number from debrief")
# debWuc$Serial_Number <- as.character(debWuc$Serial_Number)
# debWuc$Job_Control_Number <- as.character(debWuc$Job_Control_Number)
# oemWuc <- inner_join(oemWuc,debWuc)
# # fix WUC
# oemWuc$Work_Unit_Code <- as.character(oemWuc$Work_Unit_Code)
# oemWuc$Work_Unit_Code <- gsub("\\.0","",oemWuc$Work_Unit_Code)
# oemWuc$Work_Unit_Code[grep("e",oemWuc$Work_Unit_Code)] <- gsub("\\.","",oemWuc$Work_Unit_Code[grep("e",oemWuc$Work_Unit_Code)]) # scientific notation
# oemWuc$Work_Unit_Code[grep("e",oemWuc$Work_Unit_Code)] <- gsub("e\\+100","E99",oemWuc$Work_Unit_Code[grep("e",oemWuc$Work_Unit_Code)])
# # still some WUCs that are 2 characters and 10 characters - I'll leave these b/c I dont' know what to do w/ them
# # add a zero to the 4-digit wucs
# oemWuc$Work_Unit_Code[grep("^[A-Z0-9]{4}$",oemWuc$Work_Unit_Code)] <- paste0("0",oemWuc$Work_Unit_Code[grep("^[A-Z0-9]{4}$",oemWuc$Work_Unit_Code)])
# oemWuc$Work_Unit_Code <- factor(oemWuc$Work_Unit_Code)
# oemWuc$WUC_Group <- substr(oemWuc$Work_Unit_Code,1,2)

oemSN <- sqlQuery(udri, "SELECT distinct Serial_Number FROM on_equipment_maintenance")
#oemSN <- debWuc$Serial_Number
#oemSN <- unique(oemSN)
#oemSN <- as.character(oemSN)


# add geographic location - try Debrief data, then OEM data for missing ones (without sorties) b/c sometimes these have two geo locs
#tnGeoLoc <- select(debrief,Serial_Number, Geographic_Location) %>% distinct()
#tnGeoLoc <- tnGeoLoc[!(tnGeoLoc$Geographic_Location %in% "LUXC"),] # remove the LUXC
#tnGeoLoc$Geographic_Location <- factor(tnGeoLoc$Geographic_Location)
#tnHrsAndMaintAll <- left_join(tnHrsAndMaintAll, tnGeoLoc, by="Serial_Number")
#tnGeoLocMaint <- group_by(oem,Serial_Number, Geographic_Location) %>% filter(!(Geographic_Location %in% "WWYK")) %>% summarise(count = n()) %>% mutate(groupMaxCount = max(count))
#tnGeoLocMaint <- tnGeoLocMaint[tnGeoLocMaint$count == tnGeoLocMaint$groupMaxCount,] # reduces by three rows


shinyUI(fluidPage(
    fluidRow(
      column(11,
             titlePanel("Sortie Outcome and Maintenance Predictions"),
      offset=1
      )
    ),
    fluidRow(
      column(4,
        selectInput(
          'tn', 'Select Serial Number',
          #choices = paste0(as.character(oemSN$Serial_Number)," - ",oemSN$Geographic_Location), selected = "8600000094 - Ellsworth AFB"
          choices = oemSN$Serial_Number#, selected = "8600000094"
          )
        ),
      column(4,
         selectInput(
           'mType', 'Select Mission Type',
           choices = c("BONE","DARK","DITTO","FELON","FIEND","FURY","HAWK","LIGHT","OGRE","OTHER","PUMA","RAMBO","RAZOR","SABRE","SLAM","SLAYER","THUNDER","UNKNOWN")#, 
           #selected = "BONE"
         )
      ),
      column(4,
             #dateInput("sortieDate","Select Sortie Date")
             tags$br(),
             downloadLink("dataForDownload",label="Download Full Dataset")
      )
    ),
    # TS and CX deviations do not have mission codes. 313/977 attempted sorties have no mission code, including 234 tail swaps (175) and cancellations (59)
    fluidRow(
      column(12,
             textOutput("tsn")
             )
    ),
    fluidRow(
      column(12,
             tags$div(plotOutput("missionHoursPlot"),style = "height:175px")
      )
    ),
    fluidRow(
      column(12,
        helpText("Note: Tail Swap and Canceled Sorties Typically Do Not Indicate Mission Type and are Classified 'Unknown'"),
        h3("Sortie Outcome Probabilities")
      )
    ),
    #fluidRow(
    #       column(12,
    #              #tableOutput("missionTypeOutcomes"),
    #              h5(
    #                tags$div(textOutput("twoOutcomeProbs"))
    #              )
    #       )
    #),
        fluidRow(
          column(12,
                 tags$div(plotOutput("sortieOutcomePlot"),style = "height:300px")
          )
      ),
    fluidRow(
      column(12,
             h5(
               tags$div(textOutput("probOfMaint"),style="font-weight: bold")
             )
      )
    ),
    fluidRow(
      column(12,
        h3("Sortie Maintenance Probabilities"),
        plotOutput("systemMaintenanceProbabilitiesPlot")
        )
    ),
    fluidRow(
      column(4,
             tags$br(),tags$br(),tags$br(),
        selectInput(
           'actDev', 'Select Actual Mission Outcome',
           choices = c("No Deviation No Maintenance","No Deviation With Maintenance",
                       "Success but Deviation No Maintenance","Success but Deviation With Maintenance",
                       "A/C Swap","Canceled","Ground Abort","Air Abort")#paste(debDev[,1],debDev[,3],sep=" - "),
           #selected = "No Deviation No Maintenance"
        )
      ),
      column(4,
             tags$br(),tags$br(),tags$br(),
        selectInput(
          'actsys', 'Select Actual Identified System',
          choices = paste(debSys[,1],debSys[,2],sep=" - ")
          #selected = 
        )
      ),
      column(4,
             tags$br(),tags$br(),tags$br(),tags$br(),
             downloadLink("dataSubsetForDownload",label="Download Data Subset")
      )
    ),
    fluidRow(
      column(12,
             h3("Expected Per Sortie Maintenance Outcomes by Work Unit Code"),
             h4(
               tags$div(textOutput("numberOfSorites"))
               ),
             dataTableOutput("WUCavgPerSortieInSystemOutcome")
      )
    )
  )
)