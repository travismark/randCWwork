library(shiny);library(RODBC);library(plotly)

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

metricOptions <- c("Labor ManHours","Labor Calendar Hours","Job Control Numbers","Actions","Replacements With Spare",
                   "Actions Abort","Actions Air Abort","Actions Ground Abort","Break Related Maintenance",
                   "Inherent Failure Actions","Induced Failure Actions","No Defect Actions","Cannot Duplicates")

# udri <- odbcConnect(dsn="onion-udri",uid="tbaer",pw="tbaer1") # created through the Data Sources (ODBC) window described above
# oem data
#oem <- sqlQuery(udri, "SELECT * FROM on_equipment_maintenance")

shinyUI(fluidPage(
  #titlePanel('Work Unit Code Degraders'),

    wellPanel(
      fluidRow(
      column(2,
        radioButtons('oneOrTwo','Graph Type',choices = c("Pareto","Scatter Plot"))
        ),
      column(2,
        numericInput("num",label="Display Limit", value=25)
             ),
      column(3,
        selectInput(
          'mtr1', 'Select a Primary Metric',
          #choices = c("Actions","Labor Hours","Days Causing Aircraft AWP","Spares Requirements")
          choices = metricOptions
        )
      ),
      column(3,
        selectInput(
          'mtr2', 'Select a Secondary Metric',
          #choices = c("Actions","Labor Hours","Days Causing Aircraft AWP","Spares Requirements")
          choices = metricOptions, selected="Actions"
        )
      ),
      column(2,
             tags$br(),
             downloadLink("dataForDownload",label="Download Data")
      )
    )
    ),
    fluidRow(
      plotlyOutput('plot1',height="500px")
    )
  )
)