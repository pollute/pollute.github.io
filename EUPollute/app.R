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
library(shinydashboard)

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



  MaxTable = poltot%>%
    group_by(pol)%>%
    summarise("value"=max(Maximum,na.rm=TRUE))
  
  

maxvalues=data.frame(t(MaxTable[,2]))
colnames(maxvalues)=(t(MaxTable[,1]))

####################
# Shiney App
####################

# Define UI for application that draws a histogram
ui <- dashboardPage(

  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  

   # Sidebar with a slider input for number of bins
  dashboardBody(
    fluidRow(
      column(width = 3,
         selectInput("dataset",
                     "Pollutant to display:",
                     pollutants),
         
         selectInput("statistic",
                     "Statistic to display:",
                     statistic)),
       column(width=4,
              box(width=12,
         actionButton("Above", "Normalize to Limit")),
         box(width=12,
         actionButton("Reset", "Normalize to Minimum (reset)"))),
       column(width=5,
         htmlOutput("LimitText"))
         ),

      # Show a plot of the generated distribution
       
    fluidRow(
      column(height=600, width=12, 
        box(width=NULL,
            title = "European Air Pollutants (2013)",
            status = "primary",
          htmlOutput("Map")))
    )
    )
)






# Define server logic
server <- function(input, output, session) {

  #select by pollution
  datasetPol=reactive({
    switch(input$statistic,
           "Mean"=poltot%>%
                  filter(pol==input$dataset)%>%
                  mutate("value"=Mean),
           
           "Maximum"=poltot%>%
                    filter(pol==input$dataset)%>%
                    mutate("value"=Maximum)
    )
  })
  
#creates text for limits 
  Limitinfo=reactive({
    as.character(summarise(datasetPol(),max(LimitText,na.rm=TRUE)))
  })
    
  
#initialize limits
  limits=reactiveValues(data =NULL)
#reset limits after changing selection
observeEvent(input$dataset,{
  limits$data=c(min(datasetPol()$value,na.rm=TRUE),max(datasetPol()$value,na.rm=TRUE))
})
observeEvent(input$Reset,{
  limits$data=c(min(datasetPol()$value,na.rm=TRUE),max(datasetPol()$value,na.rm=TRUE))
})
observeEvent(input$statistic,{
  limits$data=c(min(datasetPol()$value,na.rm=TRUE),max(datasetPol()$value,na.rm=TRUE))
})

#reset limits after selecting above limit
observeEvent(input$Above,{
     limits$data=c(max(datasetPol()$Limit,na.rm=TRUE),
                   max(max(datasetPol()$Limit,na.rm=TRUE)+1,max(datasetPol()$value,na.rm=TRUE)))
    })
   
   #create map
   output$Map <- renderGvis({

     gvisGeoChart(
       datasetPol(),
       locationvar = "iso2c", 
       colorvar = input$statistic,
        options=list(
          title ="Concentrations",
          region='150',
          backgroundColor.fill = "#BDBDBD",
          height=450,
          width=700,
          colorAxis=paste0("{minValue:", limits$data[1],",maxValue:",limits$data[2],", colors:['#1a9641','#ffffb2', '#ef3b2c', '#99000d']}"),
          datalessRegionColor='#F5F5F5'

          ))

   })

   output$LimitText <- renderUI({
     HTML(paste("European Limits for Pollutant:", Limitinfo(), sep="<br/>"))
   })
   }
   
# Run the application
shinyApp(ui = ui, server = server)


