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

PM10 = read_excel('data/EU2013.xlsx', sheet=1)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise(`Value`=round(mean(statistic_value), digits = 2))%>%
  mutate(pol="PM10")

NO2 = read_excel('data/EU2013.xlsx', sheet=2)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise(`Value`=round(mean(statistic_value), digits = 2))%>%
  mutate(pol="O3")

O3 = read_excel('data/EU2013.xlsx', sheet=3)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise(`Value`=round(mean(statistic_value), digits = 2))%>%
  mutate(pol="O3")

PM2.5 = read_excel('data/EU2013.xlsx', sheet=4) %>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise(`Value`=round(mean(statistic_value), digits = 2))%>%
  mutate(pol="PM2.5")

BaP = read_excel('data/EU2013.xlsx', sheet=5)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise(`Value`=round(mean(statistic_value), digits = 2))%>%
  mutate(pol="BaP")

SO2 = read_excel('data/EU2013.xlsx', sheet=6)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise(`Value`=round(mean(statistic_value), digits = 2))%>%
  mutate(pol="SO2")

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
                     pollutants)
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
   datasetInput <- reactive({
     switch(input$dataset,
            "PM10" = PM10,
            "NO2" = NO2,
            "O3" = O3,
            "PM2.5" = PM2.5,
            "BaP" = BaP,
            "SO2" = SO2)
     })
   
   output$view <- renderGvis({
     gvisGeoChart(
        datasetInput(),locationvar = "iso2c", colorvar = "Value",
        options=list(
          title ="Concentrations",
          region='150',
          width=600, height=400,
          backgroundColor.fill = "#BDBDBD",
          colorAxis="{colors:['#FFEBEE', '#F44336']}"
          ))
      
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

