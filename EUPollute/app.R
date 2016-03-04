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

PM10 = read_excel('data/EU2013.xlsx', sheet=1)%>%
  mutate(iso2c= `country iso code`)%>%
  group_by(iso2c)%>%
  summarise("Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
  mutate(pol="PM10")%>%
  mutate(unit="microgams per cubic meter")

NO2 = read_excel('data/EU2013.xlsx', sheet=2)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise("Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
  mutate(pol="NO2")%>%
  mutate(unit="microgams per cubic meter")

O3 = read_excel('data/EU2013.xlsx', sheet=3)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise("Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
  mutate(pol="O3")%>%
  mutate(unit="microgams per cubic meter")

PM2.5 = read_excel('data/EU2013.xlsx', sheet=4) %>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise("Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
  mutate(pol="PM2.5")%>%
  mutate(unit="microgams per cubic meter")

BaP = read_excel('data/EU2013.xlsx', sheet=5)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise("Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
  mutate(pol="BaP")%>%
  mutate(unit="nanogams per cubic meter")

SO2 = read_excel('data/EU2013.xlsx', sheet=6)%>%
  mutate(iso2c=`country iso code`)%>%
  group_by(iso2c)%>%
  summarise("Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
  mutate(pol="SO2")%>%
  mutate(unit="microgams per cubic meter")

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
       datasetInput(),locationvar = "iso2c", colorvar = input$pollutant, hovervar="unit",
        
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

