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

## Location Data
#geoLoc <- sqlQuery(udri, "SELECT * from geoloc_code")
#names(geoLoc) <- c("Geographic_Location","Location_Name","City","State")

# udri <- odbcConnect(dsn="onion-udri",uid="tbaer",pw="tbaer1") # created through the Data Sources (ODBC) window described above
# oem data
oemSN <- sqlQuery(udri, "SELECT distinct Serial_Number FROM on_equipment_maintenance")
oemSN$Serial_Number <- as.character(oemSN$Serial_Number)
debDev <- sqlQuery(udri, "SELECT * FROM deviation_code")
#debSubsys <- sqlQuery(udri, "SELECT distinct Subsystem_Work_Unit_Code, subsystem_wuc_description FROM debrief")
#debSubsys$Subsystem_Work_Unit_Code <- as.character(debSubsys$Subsystem_Work_Unit_Code)
#debSubsys$Subsystem_Work_Unit_Code[is.na(debSubsys$Subsystem_Work_Unit_Code)] <- "NONE"
#debSubsys$Subsystem_Work_Unit_Code <- gsub("\\.0","",debSubsys$Subsystem_Work_Unit_Code)
debSubsys <- sqlQuery(udri, "SELECT * FROM work_unit_code where length(code) = 2")



# add geographic location - try Debrief data, then OEM data for missing ones (without sorties) b/c sometimes these have two geo locs
#tnGeoLoc <- select(debrief,Serial_Number, Geographic_Location) %>% distinct()
#tnGeoLoc <- tnGeoLoc[!(tnGeoLoc$Geographic_Location %in% "LUXC"),] # remove the LUXC
#tnGeoLoc$Geographic_Location <- factor(tnGeoLoc$Geographic_Location)
#tnHrsAndMaintAll <- left_join(tnHrsAndMaintAll, tnGeoLoc, by="Serial_Number")
#tnGeoLocMaint <- group_by(oem,Serial_Number, Geographic_Location) %>% filter(!(Geographic_Location %in% "WWYK")) %>% summarise(count = n()) %>% mutate(groupMaxCount = max(count))
#tnGeoLocMaint <- tnGeoLocMaint[tnGeoLocMaint$count == tnGeoLocMaint$groupMaxCount,] # reduces by three rows


shinyUI(fluidPage(
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
           choices = c("BONE","DARK","DITTO","FELON","FIEND","FURY","HAWK","LIGHT","OGRE","PUMA","RAMBO","RAZOR","SABRE","SLAM","SLAYER")#, 
           #selected = "BONE"
         )
      ),
      column(4,
             dateInput("sortieDate","Select Sortie Date")
      )
    ),
    # TS and CX deviations do not have mission codes. 313/977 attempted sorties have no mission code, including 234 tail swaps (175) and cancellations (59)
    fluidRow(
      column(12,
             tags$div(plotOutput("missionHoursPlot"),style = "height:175px")
      )
    ),
    fluidRow(
      column(6,
        h3("Sortie Outcome Probabilities"),
        fluidRow(
           column(12,
                  tableOutput("missionTypeOutcomes")
#             fluidRow(
#               column(3,
#                      tags$div("Successful\nSorties:") # ,style = "text-decoration: underline"
#                      ),
#               column(3,
#                      textOutput("tabMisOutSuc")
#               ),
#               column(3,
#                      "Unsuccessful\nSorties:"
#               ),
#               column(3,
#                      textOutput("tabMisOutUnsuc")
#               )              
#             )
           )
        )
      ),
      column(6,
        h3("Sortie Maintenance Probabilities"),
        fluidRow(
          column(12,
                 plotOutput("systemMaintenanceProbabilities")
                 )
        )
      )
    ),
    fluidRow(
      column(4,
             tags$br(),tags$br(),tags$br(),
        selectInput(
           'actDev', 'Select Actual Deviation',
           choices = paste(debDev[,1],debDev[,3],sep=" - "),
           selected = "GA"
        )
      ),
      column(8,
             tags$br(),tags$br(),tags$br(),
        selectInput(
          'actSubsys', 'Select Actual Identified System',
          choices = paste(debSubsys[,1],debSubsys[,2],sep=" - "),
          selected = "430"
        )
      )
    ),
    fluidRow(
      column(12,
        h3("Expected Maintenance Outcomes by Work Unit Code")
      )
    ),
    fluidRow(
      column(4,
        tags$br(),tags$br(),tags$br(),
        actionButton("addSortieButton","Add Sortie to Schedule")
      ),
      column(8,
        tableOutput("plannedSorts")
      )
    ),
    fluidRow(
      column(12,
        h3("Fleet Quick Predictions")
      )
    )
  )
)