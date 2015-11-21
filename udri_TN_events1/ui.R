library(shiny)
library(plotly)
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
oem <- sqlQuery(udri, "SELECT distinct(Serial_Number) FROM on_equipment_maintenance")


shinyUI(fluidPage(#theme = "SoF_CSS",
    fluidRow(
      column(4,
        selectizeInput(
          'tns', 'Select Serial Numbers (max 10)',
          choices = unique(oem$Serial_Number), selected = 8600000094, multiple = TRUE,
          options = list(maxItems = 10)
        )
      ),
      column(4,
        dateRangeInput(
          'dts', label = "Select Date Range",
          start = '2014-06-01', end = '2014-07-31',
          min = min(oem$Transaction_Date), max = max(oem$Transaction_Date)
        )
      ),
      column(3,
             tags$div(tags$br()),
             downloadLink("dataForDownload",label="Download Data Subset"),
             offset=1
      )
    ),
    fluidRow(
      column(12,
#      plotOutput('plot1'),
        plotlyOutput("timelinePlot")
      )
    )
  )
)