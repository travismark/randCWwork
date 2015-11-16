library(shiny)
library(RODBC)

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

# udri <- odbcConnect(dsn="onion-udri",uid="tbaer",pw="tbaer1") # created through the Data Sources (ODBC) window described above
# oem data
oem <- sqlQuery(udri, "SELECT * FROM on_equipment_maintenance")

shinyUI(fluidPage(
  titlePanel('Achieved Hours and Cannibalization Downtime'),
    fluidRow(
      selectizeInput(
        'tns', 'Select Tail Numbers (max 10)',
        choices = unique(oem$Serial_Number), selected = 8600000094, multiple = TRUE,
        options = list(maxItems = 10)
      )
    ),
    fluidRow(
#      plotOutput('plot1'),
      plotlyOutput("timelinePlot")
    )
  )
)

# sidebarLayout(
#   sidebarPanel(
#     selectizeInput(
#       'tns', 'Select Tail Numbers (max 10)',
#       choices = unique(oem$Serial_Number), selected = 8600000094, multiple = TRUE,
#       options = list(maxItems = 10)
#     )
#   ),
#   mainPanel(
#     #      plotOutput('plot1'),
#     plotlyOutput("timelinePlot")
#   )
# )
# )
# )