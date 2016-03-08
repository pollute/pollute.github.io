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

#setup data table for map
for  (i in 1:length(pollutants)){
  DataTable = read_excel('data/EU2013.xlsx', sheet=i)%>%
    mutate(iso2c= `country iso code`)%>%
    group_by(iso2c)%>%
    summarise("Limit"=max(Limit),"LimitText"=max(LimitText),"Mean"=round(mean(statistic_value), digits = 2),"Maximum"=round(max(statistic_value), digits = 2))%>%
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
         
         selectInput("statistic",
                     "Statistic to display:",
                     statistic),
         
         sliderInput("cutoff",
                      "Pollutant Cuttoff",
                      min=0,
                      max=100,
                      value=0),
         
         actionButton("LimitButton", "Set Cutoff to Limit")
          ),

      # Show a plot of the generated distribution
      mainPanel(
        htmlOutput("view")
        )
      )
   )
)

# Define server logic
server <- shinyServer(function(input, output, session) {
  
  #create data to reset slider
  sliderMax=reactive({
    as.character(switch(input$statistic,
                        
                        'Mean'=poltot%>%
                          filter(pol==input$dataset)%>%
                          summarise(Max=max(Mean,na.rm=TRUE)),
                 
                        'Maximum'=poltot%>%
                          filter(pol==input$dataset)%>%
                          summarise(Max=max(Maximum,na.rm=TRUE))
    ))
  })
  
  sliderValue=reactive({
    as.double(filter(poltot,pol==input$dataset)%>%
                   summarise(Limit=max(Limit,na.rm=TRUE)))
  })
  
  sliderLabel=reactive({
    as.character(filter(poltot,pol==input$dataset)%>%
                   summarise(Lable=max(LimitText,na.rm=TRUE)))
  })
  

  #have slider update to inputs
  observe({
    updateSliderInput(session, "cutoff",value=0,label=paste("Cutoff Level:",sliderLabel()), max=sliderMax())
  })
  
  
  SliderV=eventReactive(input$LimitButton, {sliderValue()
  })

  observe({
    updateSliderInput(session, "cutoff",value=SliderV())
  })
  
  
  #create dataset for map   
  datasetInput=reactive({
    switch(input$statistic,
           
           'Mean'=poltot%>%
             filter(pol==input$dataset)%>%
             filter(Mean >= input$cutoff),
    
           'Maximum'=poltot%>%
            filter(pol==input$dataset)%>%
            filter(Maximum >= input$cutoff))
    
  })
  

  #create map
  

    
   output$view <- renderGvis({
     
     gvisGeoChart(

       datasetInput(),locationvar = "iso2c", colorvar = input$statistic,
        options=list(
          title ="Concentrations",
          region='150',
          width=600, height=800,
          backgroundColor.fill = "#BDBDBD",
          colorAxis="{colors:['#FFEBEE', '#F44336']}",
          enableRegionInteractivity=TRUE
          ))

   })
   
})

# Run the application
shinyApp(ui = ui, server = server)


