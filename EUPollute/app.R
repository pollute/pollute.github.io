#
# Authors: Elise, Steph, Mitchell
# Description: This is a Shiney app that takes pollutant data for five pollutants across the EU and displays concentration level by country. You can select which pollutant to display by dropdown.
#

###############
# Data Set up
###############

library(shiny)
library(googleVis)
library(RCurl)
library(readxl)
library(dplyr)

pollutants = c("PM10", "NO2", "O3", "PM2.5", "BaP", "SO2")
statistic= c('Mean','Maximum')

for  (i in 1:length(pollutants)){
  DataTable = read_excel('data/EU2013.xlsx', sheet=i)%>%
    mutate(iso2c= `country iso code`)%>%
    group_by(iso2c)%>%
    summarise("Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
    mutate(pol=pollutants[i])
  
  assign(pollutants[i],DataTable)
}

poltot = bind_rows(list(PM10, NO2, O3, PM2.5, BaP, SO2))

####################
# Shiney App
####################

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

   # Application title
   titlePanel("Common EU Pollutants (2013)"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         selectInput("dataset",
                     "Pollutant to display:",
                     pollutants),
         selectInput("pollutant",
                     "Statistic to display:",
                     statistic)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        htmlOutput("view")
        )
      )
   )
)

# Define server logic
server <- shinyServer(function(input, output) {

  datasetInput = reactive({
     filter(poltot,pol==input$dataset)
     })
  
   output$view <- renderGvis({
     gvisGeoChart(

       datasetInput(),locationvar = "iso2c", colorvar = input$pollutant,
        
 
        options=list(
          title ="Concentrations",
          region='150',
          width=600, height=800,
          backgroundColor.fill = "#BDBDBD",
          colorAxis="{colors:['#FFEBEE', '#F44336']}"
          ))

   })
})

# Run the application
shinyApp(ui = ui, server = server)

